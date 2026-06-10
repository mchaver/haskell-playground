# itch-codec

A small encoder/decoder for a subset of NASDAQ TotalView-ITCH 5.0 
(binary market data feed for NASDAQ order book events).

```
make test    # round-trip + malformed-input property suite
make run     # encode some messages, dump bytes, decode them back
make check   # build + test + lint + format-check
```

## What it does

`decode` and `encode` (`src/ITCH/Codec.hs`) convert between raw frames and a
typed `Message`:

```haskell
encode :: Message      -> ByteString
decode :: ByteString   -> Either DecodeError Message
```

ITCH messages are fixed-width, big-endian records, each introduced by a
one-byte message type. The implemented subset:

| Type | Message | Bytes | Notes |
|---|---|--:|---|
| `S` | System Event | 12 | market open/close lifecycle |
| `A` | Add Order | 36 | resting order added to the book |
| `E` | Order Executed | 31 | resting order (partly) filled |
| `C` | Order Executed With Price | 36 | execution at a price + printable flag |
| `X` | Order Cancel | 23 | partial cancel |
| `D` | Order Delete | 19 | full cancel |
| `U` | Order Replace | 35 | cancel-replace (orig ref → new ref) |
| `P` | Trade (non-cross) | 44 | non-displayable execution |

Every message shares a `Header` (stock locate, tracking number, 48-bit
nanosecond timestamp) immediately after the type byte.

## Design: typed failures and unrepresentable bad states

Decoding returnes `Either DecodeError Message` where `DecodeError` is defined 
as:

```haskell
data DecodeError
  = EmptyInput                    -- no message-type byte
  | UnknownMessageType !Word8     -- leading byte isn't a type we know
  | Truncated !Char !Int !Int     -- type, expected length, actual length
  | TrailingBytes !Int            -- a fixed-width message had N extra bytes
  | MalformedField !Char !String  -- a field value was out of its domain
```

Because every message has a known fixed length, the decoder classifies framing
errors (`Truncated` / `TrailingBytes`) from the length alone, before parsing. 
Only a value-domain violation (e.g. a Buy/Sell byte that is neither `B` nor `S`)
surfaces as `MalformedField`.

Illegal states aren't representable. Two fields carry invariants that the
type system enforces at construction, which is what makes re-encoding
byte-exact:

- `Timestamp` wraps a 48-bit value. Its constructor is hidden and `mkTimestamp`
  rejects anything that wouldn't fit the 6 wire bytes.
- `Stock` is exactly 8 bytes. Its constructor is hidden and `mkStock` enforces
  the width.

Serialization uses [`cereal`](https://hackage.haskell.org/package/cereal).

## The properties (`test/Main.hs`)

| Property | What it guarantees |
|---|---|
| **Round-trip** | `decode (encode m) == Right m` for every message (via Hedgehog's `tripping`). |
| **Truncated → `Truncated`** | Dropping any non-empty suffix of a frame is reported as `Truncated`, never a partial parse or exception. |
| **Trailing → `TrailingBytes`** | Extra bytes after a complete message are reported with the exact surplus count. |
| **Unknown type** | Any leading byte outside the known set yields `UnknownMessageType` carrying that byte. |
| **Empty → `EmptyInput`** | Empty input is its own typed failure. |

## Developing

| Command | What it does |
|---|---|
| `make build` | Compile library, demo, and tests. |
| `make test` | Run the property suite. |
| `make run` | Run the encode/decode demo. |
| `make lint` | HLint over `src app test`. |
| `make format` / `make format-check` | Fourmolu (rewrite / verify). |
| `make check` | `build` + `test` + `lint` + `format-check`. |

Lint and formatting rules are shared across the repo and live one level up:
[`../.hlint.yaml`](../.hlint.yaml) and [`../fourmolu.yaml`](../fourmolu.yaml).

## Out of scope

Since this is just exploratory code, meant for reading and understanding, it
does not offer a full feed handler. 

The following are omitted:
- stock directory message
- MPID message
- cross/NOII message
- SoupBinTCP, MoldUDP64 session and framing layers that carry ITCH in production.
- book reconstruction from the event stream.
