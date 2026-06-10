# matching-engine

A small, correctness-first **central limit order book (CLOB)** and matching
engine in Haskell. The goal is not feature breadth — it is to demonstrate the
two things an exchange engine lives or dies by: the matching rules are *correct*,
and that correctness is *pinned down by machine-checked invariants* rather than a
handful of hand-written examples.

```
make test    # run the property suite (the point of the project)
make run     # watch a scripted order flow match
make check   # what CI runs: build + test + lint + format-check
```

See [Developing](#developing) for the full task list and tool setup.

## What it does

- Price–time priority matching (best price first; FIFO within a price level).
- Limit and market orders.
- Time-in-force: GTC (rest the remainder), IOC (cancel the remainder),
  FOK (all-or-nothing).
- Partial fills, multi-level sweeps, and resting of unfilled remainders.

A single entry point, `match :: NewOrder -> Book -> MatchResult`, takes an
incoming order and a book and returns the trades, the taker's outcome, and the
new book. The engine is pure: no `IO`, no mutable state, so it is trivially
testable and deterministically replayable.

## Design: make illegal states unrepresentable

The type system does real work here (`src/OrderBook/Types.hs`):

- **`Quantity` is strictly positive by construction.** Its constructor is
  hidden; the only way to make one is `mkQuantity`, which rejects `<= 0`. So a
  zero-quantity trade or a fully-filled order left resting on the book is not a
  bug you can have — it is a value you cannot build.
- **A resting order always has a limit price.** `Resting` has a `Price` field,
  not a `Maybe Price`. Market orders never rest, and there is no field in which a
  "resting market order" could even be stored.
- **Prices are integer ticks, never floats.** Order matching compares prices for
  exact equality and ordering; floating point has no place in that.
- **Time priority is an explicit `SeqNo`**, handed out monotonically as orders
  rest, rather than something implied by data-structure ordering.

The book itself (`src/OrderBook/Book.hs`) is two `Map Price (Seq Resting)` —
price priority comes from walking the map best-first, time priority from the
FIFO `Seq` at each level.

## The invariants (`test/Main.hs`)

Each property runs hundreds of generated order flows; on failure Hedgehog
shrinks to a minimal counterexample.

| Property | What it guarantees |
|---|---|
| **Conservation of quantity** | `bookBefore − bookAfter == traded − restedByTaker`. No units are created or destroyed by a match. |
| **Book is never crossed** | After *every* order in a flow, best bid `<` best ask. A crossed book means a trade was missed. |
| **Taker price protection** | A buyer never trades above its limit; a seller never below it. |
| **Price priority of fills** | A taker's trade prices improve monotonically away from it — cheapest asks / richest bids consumed first. |
| **Fill-or-kill is atomic** | A FOK order either fills in full or is rejected with **no trades and the book byte-for-byte unchanged** — never a partial. |
| **No zero-qty resting order** | Every order left on the book has positive size (the engine never strands a filled maker). |

## Tradeoffs and what's deliberately out of scope

This is a focused core, not a production exchange. Conscious omissions:

- **No persistence / sequencing layer.** The engine is the pure inner loop; an
  event-sourced journal (append every accepted `NewOrder`, rebuild book state by
  replay, reconcile against a snapshot) is the natural next layer and the obvious
  way to get durability and crash recovery. The purity here is what makes that
  replay deterministic.
- **`Map`/`Seq`, not a latency-tuned structure.** Correct and `O(log n)` per
  level is the right *first* target. A realistic next step is to benchmark
  (criterion) a flat array of price levels and an intrusive FIFO, and let numbers
  — not vibes — justify the more complex structure.
- **Single book, single thread.** Concurrency belongs at the boundary (one
  writer per book, a ring buffer in front), not inside the matching logic.
- **No self-trade prevention, no order modification, no fees/rebates, no
  auctions/halts.** All are additive on top of this core.

## Developing

### Prerequisites

GHC and Cabal (via [GHCup](https://www.haskell.org/ghcup/)) build and run
everything. Linting and formatting need two extra tools on your `PATH`:

| Tool | Install |
|---|---|
| [HLint](https://github.com/ndmitchell/hlint) | `cabal install hlint` |
| [Fourmolu](https://github.com/fourmolu/fourmolu) | `cabal install fourmolu`, or grab a prebuilt binary from its releases |

The `Makefile` invokes `cabal`, `hlint`, and `fourmolu` by name; override any of
them if they live elsewhere, e.g. `make lint HLINT=~/.cabal/bin/hlint`.

### Tasks

Run `make` (or `make help`) to list them:

| Command | What it does |
|---|---|
| `make build` | Compile the library, demo, and test suite. |
| `make test` | Run the Hedgehog property suite. |
| `make run` | Run the scripted matching demo. |
| `make repl` | Open a GHCi session on the library. |
| `make lint` | Run HLint over `src app test`. |
| `make format` | Reformat all sources in place with Fourmolu. |
| `make format-check` | Fail (without writing) if anything is unformatted. |
| `make check` | `build` + `test` + `lint` + `format-check` — the CI gate. |
| `make clean` | Remove build artifacts (`cabal clean`). |

### Style and lint rules

Two checked-in config files keep the code consistent and steer it away from
common Haskell footguns:

- **`fourmolu.yaml`** — formatting (2-space indent, leading commas, `-- |`
  Haddock). `make format` rewrites to this; `make format-check` enforces it.
- **`.hlint.yaml`** — house rules on top of HLint's defaults, each with a
  message pointing at the fix:
  - **No partial functions** from the Prelude/`base` (`head`, `tail`, `fromJust`,
    `read`, `Data.Map.!`, …) — they throw at runtime.
  - **No `error` / `undefined`** in pure code — model failure with `Maybe`/
    `Either` or smart constructors like `mkQuantity` instead.
  - **No lazy left folds or quadratic/lazy list helpers** (`foldl`, `nub`,
    `genericLength`, …) — use `foldl'`, `nubOrd`, etc.
  - **No lazy container/monad modules** (`Data.Map`, `Control.Monad.State`, …) —
    import the `.Strict` variants.

  To permit a banned function in one module, add it to that rule's `within:`
  list in `.hlint.yaml`.

## Layout

```
src/OrderBook/Types.hs   -- domain types; illegal states made unrepresentable
src/OrderBook/Book.hs    -- the book and the matching engine
test/Main.hs             -- Hedgehog property suite (the invariants above)
app/Main.hs              -- a scripted demo that prints trades and the book
.hlint.yaml              -- lint house rules (partial fns, error, lazy folds, ...)
fourmolu.yaml            -- formatting config
Makefile                 -- build / test / lint / format tasks
```
