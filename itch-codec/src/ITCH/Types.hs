{-# LANGUAGE DerivingStrategies #-}

-- | Domain types for a subset of the NASDAQ TotalView-ITCH 5.0 protocol.
--
-- The wire format is a stream of fixed-width, big-endian records, each
-- introduced by a one-byte message type. The types here mirror that format
-- while making the protocol's invariants unrepresentable as bugs:
--
--   * 'Timestamp' is a 48-bit nanoseconds-since-midnight value. Its constructor
--     is hidden so a value that cannot fit the 6 wire bytes cannot be built.
--   * 'Stock' is exactly 8 bytes (space-padded alpha on the wire). Its
--     constructor is hidden so the fixed width always holds.
--
-- Both invariants are what make re-encoding byte-exact, so they are enforced at
-- construction rather than checked at encode time.
module ITCH.Types
  ( -- * Field types
    StockLocate (..)
  , TrackingNumber (..)
  , Timestamp
  , mkTimestamp
  , timestampNanos
  , maxTimestamp
  , OrderRef (..)
  , MatchNumber (..)
  , Shares (..)
  , Price (..)
  , Stock
  , mkStock
  , stockBytes
  , stockWidth

    -- * Enumerated fields
  , Side (..)
  , SystemEventCode (..)
  , Printable (..)

    -- * Messages
  , Header (..)
  , Message (..)

    -- * Decode failures
  , DecodeError (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16, Word32, Word64, Word8)

-- | Index assigned by NASDAQ to a security for the trading day.
newtype StockLocate = StockLocate Word16
  deriving stock (Eq, Ord, Show)

-- | Nasdaq-internal tracking number.
newtype TrackingNumber = TrackingNumber Word16
  deriving stock (Eq, Ord, Show)

-- | Nanoseconds since midnight, carried in 6 bytes on the wire. The constructor
-- is hidden; 'mkTimestamp' rejects anything that would not fit those 6 bytes.
newtype Timestamp = Timestamp Word64
  deriving stock (Eq, Ord, Show)

-- | The largest representable 'Timestamp' (@2^48 - 1@).
maxTimestamp :: Word64
maxTimestamp = 0xFFFFFFFFFFFF

mkTimestamp :: Word64 -> Maybe Timestamp
mkTimestamp n
  | n <= maxTimestamp = Just (Timestamp n)
  | otherwise = Nothing

timestampNanos :: Timestamp -> Word64
timestampNanos (Timestamp n) = n

-- | Day-unique order identifier.
newtype OrderRef = OrderRef Word64
  deriving stock (Eq, Ord, Show)

-- | Day-unique match identifier for an execution.
newtype MatchNumber = MatchNumber Word64
  deriving stock (Eq, Ord, Show)

-- | A share quantity.
newtype Shares = Shares Word32
  deriving stock (Eq, Ord, Show)

-- | A price in integer ticks (ITCH uses 4 implied decimal places).
newtype Price = Price Word32
  deriving stock (Eq, Ord, Show)

-- | An 8-byte alpha ticker field, space-padded on the wire. The constructor is
-- hidden so the fixed width is guaranteed; build one with 'mkStock'.
newtype Stock = Stock ByteString
  deriving stock (Eq, Ord, Show)

-- | The fixed width of a 'Stock' field in bytes.
stockWidth :: Int
stockWidth = 8

mkStock :: ByteString -> Maybe Stock
mkStock bs
  | BS.length bs == stockWidth = Just (Stock bs)
  | otherwise = Nothing

stockBytes :: Stock -> ByteString
stockBytes (Stock bs) = bs

-- | Side of an order. On the wire: @\'B\'@ / @\'S\'@.
data Side = Buy | Sell
  deriving stock (Eq, Show, Enum, Bounded)

-- | System event codes (message type @\'S\'@). On the wire: a single char.
data SystemEventCode
  = -- | @\'O\'@
    StartOfMessages
  | -- | @\'S\'@
    StartOfSystemHours
  | -- | @\'Q\'@
    StartOfMarketHours
  | -- | @\'M\'@
    EndOfMarketHours
  | -- | @\'E\'@
    EndOfSystemHours
  | -- | @\'C\'@
    EndOfMessages
  deriving stock (Eq, Show, Enum, Bounded)

-- | Whether an execution is printable to the public trade feed. On the wire:
-- @\'Y\'@ / @\'N\'@.
data Printable
  = NotPrintable
  | IsPrintable
  deriving stock (Eq, Show, Enum, Bounded)

-- | The fields every message in this subset shares, in wire order, after the
-- one-byte message type.
data Header = Header
  { stockLocate :: !StockLocate
  , trackingNumber :: !TrackingNumber
  , timestamp :: !Timestamp
  }
  deriving stock (Eq, Show)

-- | A decoded ITCH message. Constructors are listed with their wire type byte.
data Message
  = -- | @\'S\'@ System Event.
    SystemEvent !Header !SystemEventCode
  | -- | @\'A\'@ Add Order (no MPID attribution). (MPID Market Participant Identifier)
    AddOrder !Header !OrderRef !Side !Shares !Stock !Price
  | -- | @\'E\'@ Order Executed.
    OrderExecuted !Header !OrderRef !Shares !MatchNumber
  | -- | @\'C\'@ Order Executed With Price.
    OrderExecutedWithPrice !Header !OrderRef !Shares !MatchNumber !Printable !Price
  | -- | @\'X\'@ Order Cancel (partial).
    OrderCancel !Header !OrderRef !Shares
  | -- | @\'D\'@ Order Delete (full).
    OrderDelete !Header !OrderRef
  | -- | @\'U\'@ Order Replace (original ref, new ref, shares, price).
    OrderReplace !Header !OrderRef !OrderRef !Shares !Price
  | -- | @\'P\'@ Trade (non-cross).
    Trade !Header !OrderRef !Side !Shares !Stock !Price !MatchNumber
  deriving stock (Eq, Show)

-- | Everything that can go wrong decoding a single message frame. Decoding
-- never throws; it returns one of these instead.
data DecodeError
  = -- | The input was empty (no message-type byte).
    EmptyInput
  | -- | The leading byte is not a message type this codec knows.
    UnknownMessageType !Word8
  | -- | Fewer bytes than the message type requires: type, expected total length,
    -- actual total length.
    Truncated !Char !Int !Int
  | -- | More bytes than the (fixed-width) message needs: count of extra bytes.
    TrailingBytes !Int
  | -- | A field held a value outside its domain (e.g. a Buy/Sell byte that is
    -- neither @\'B\'@ nor @\'S\'@): message type and a detail string.
    MalformedField !Char !String
  deriving stock (Eq, Show)
