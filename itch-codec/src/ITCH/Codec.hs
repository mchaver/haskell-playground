-- | Encoder and decoder for the ITCH 5.0 message subset in "ITCH.Types".
--
-- Every message is a fixed-width, big-endian record introduced by a one-byte
-- message type. 'encode' is a total function; 'decode' is total in the sense
-- that it never throws — malformed input becomes a typed 'DecodeError'.
--
-- The decoder uses the protocol's fixed lengths to classify framing failures
-- ('Truncated' / 'TrailingBytes') precisely, before parsing the body. Only
-- value-domain failures (a bad enumerated byte) surface as 'MalformedField'.
module ITCH.Codec
  ( encode
  , decode
  , knownMessageTypes
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (listToMaybe)
import Data.Serialize
  ( Get
  , Put
  , getByteString
  , getWord16be
  , getWord32be
  , getWord64be
  , getWord8
  , putByteString
  , putWord16be
  , putWord32be
  , putWord64be
  , putWord8
  , runGet
  , runPut
  )
import Data.Word (Word64)

import ITCH.Types
  ( DecodeError (..)
  , Header (..)
  , MatchNumber (..)
  , Message (..)
  , OrderRef (..)
  , Price (..)
  , Printable (..)
  , Shares (..)
  , Side (..)
  , Stock
  , StockLocate (..)
  , SystemEventCode (..)
  , Timestamp
  , TrackingNumber (..)
  , mkStock
  , mkTimestamp
  , stockBytes
  , timestampNanos
  )

-- ---------------------------------------------------------------------------
-- Message table — the single source of truth for type byte, length, and parser
-- ---------------------------------------------------------------------------

-- | Each known message: its wire type char, total length in bytes (including
-- the type byte), and the parser for everything after the type byte.
specs :: [(Char, Int, Get Message)]
specs =
  [ ('S', 12, SystemEvent <$> getHeader <*> getSystemEvent)
  ,
    ( 'A'
    , 36
    , AddOrder
        <$> getHeader
        <*> getOrderRef
        <*> getSide
        <*> getShares
        <*> getStock
        <*> getPrice
    )
  , ('E', 31, OrderExecuted <$> getHeader <*> getOrderRef <*> getShares <*> getMatchNumber)
  ,
    ( 'C'
    , 36
    , OrderExecutedWithPrice
        <$> getHeader
        <*> getOrderRef
        <*> getShares
        <*> getMatchNumber
        <*> getPrintable
        <*> getPrice
    )
  , ('X', 23, OrderCancel <$> getHeader <*> getOrderRef <*> getShares)
  , ('D', 19, OrderDelete <$> getHeader <*> getOrderRef)
  , ('U', 35, OrderReplace <$> getHeader <*> getOrderRef <*> getOrderRef <*> getShares <*> getPrice)
  ,
    ( 'P'
    , 44
    , Trade
        <$> getHeader
        <*> getOrderRef
        <*> getSide
        <*> getShares
        <*> getStock
        <*> getPrice
        <*> getMatchNumber
    )
  ]

-- | The message type chars this codec understands, in wire order.
knownMessageTypes :: [Char]
knownMessageTypes = [c | (c, _, _) <- specs]

messageSpec :: Char -> Maybe (Int, Get Message)
messageSpec c = listToMaybe [(n, p) | (c', n, p) <- specs, c' == c]

-- ---------------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------------

-- | Decode a single message frame. Never throws: every failure mode is a
-- typed 'DecodeError'.
decode :: ByteString -> Either DecodeError Message
decode bs =
  case BS.uncons bs of
    Nothing -> Left EmptyInput
    Just (tagByte, body) ->
      let tag = toEnum (fromIntegral tagByte)
       in case messageSpec tag of
            Nothing -> Left (UnknownMessageType tagByte)
            Just (total, parser) ->
              let expectedBody = total - 1
                  actualBody = BS.length body
               in if actualBody < expectedBody
                    then Left (Truncated tag total (BS.length bs))
                    else
                      if actualBody > expectedBody
                        then Left (TrailingBytes (actualBody - expectedBody))
                        else case runGet parser body of
                          Right m -> Right m
                          Left e -> Left (MalformedField tag e)

getHeader :: Get Header
getHeader = Header <$> getStockLocate <*> getTrackingNumber <*> getTimestamp

getStockLocate :: Get StockLocate
getStockLocate = StockLocate <$> getWord16be

getTrackingNumber :: Get TrackingNumber
getTrackingNumber = TrackingNumber <$> getWord16be

getTimestamp :: Get Timestamp
getTimestamp = do
  n <- getWord48be
  -- A 48-bit read always fits, so this is total; the branch guards the type.
  maybe (fail "timestamp exceeds 48 bits") pure (mkTimestamp n)

getStock :: Get Stock
getStock = do
  bs <- getByteString 8
  maybe (fail "stock field must be 8 bytes") pure (mkStock bs)

getOrderRef :: Get OrderRef
getOrderRef = OrderRef <$> getWord64be

getMatchNumber :: Get MatchNumber
getMatchNumber = MatchNumber <$> getWord64be

getShares :: Get Shares
getShares = Shares <$> getWord32be

getPrice :: Get Price
getPrice = Price <$> getWord32be

getSide :: Get Side
getSide = do
  w <- getWord8
  case w of
    0x42 -> pure Buy -- 'B'
    0x53 -> pure Sell -- 'S'
    _ -> fail ("Buy/Sell indicator was " <> show w <> ", expected 'B' or 'S'")

getSystemEvent :: Get SystemEventCode
getSystemEvent = do
  w <- getWord8
  case w of
    0x4F -> pure StartOfMessages -- 'O'
    0x53 -> pure StartOfSystemHours -- 'S'
    0x51 -> pure StartOfMarketHours -- 'Q'
    0x4D -> pure EndOfMarketHours -- 'M'
    0x45 -> pure EndOfSystemHours -- 'E'
    0x43 -> pure EndOfMessages -- 'C'
    _ -> fail ("unrecognised system event code " <> show w)

getPrintable :: Get Printable
getPrintable = do
  w <- getWord8
  case w of
    0x59 -> pure IsPrintable -- 'Y'
    0x4E -> pure NotPrintable -- 'N'
    _ -> fail ("printable flag was " <> show w <> ", expected 'Y' or 'N'")

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Encode a message to its fixed-width wire form. Inverse of 'decode' on
-- well-formed input (see the round-trip property in the test suite).
encode :: Message -> ByteString
encode = runPut . putMessage

putMessage :: Message -> Put
putMessage msg = case msg of
  SystemEvent h ev -> do
    putTag 'S'
    putHeader h
    putSystemEvent ev
  AddOrder h ref side shares stock price -> do
    putTag 'A'
    putHeader h
    putOrderRef ref
    putSide side
    putShares shares
    putStock stock
    putPrice price
  OrderExecuted h ref shares match -> do
    putTag 'E'
    putHeader h
    putOrderRef ref
    putShares shares
    putMatchNumber match
  OrderExecutedWithPrice h ref shares match printable price -> do
    putTag 'C'
    putHeader h
    putOrderRef ref
    putShares shares
    putMatchNumber match
    putPrintable printable
    putPrice price
  OrderCancel h ref shares -> do
    putTag 'X'
    putHeader h
    putOrderRef ref
    putShares shares
  OrderDelete h ref -> do
    putTag 'D'
    putHeader h
    putOrderRef ref
  OrderReplace h origRef newRef shares price -> do
    putTag 'U'
    putHeader h
    putOrderRef origRef
    putOrderRef newRef
    putShares shares
    putPrice price
  Trade h ref side shares stock price match -> do
    putTag 'P'
    putHeader h
    putOrderRef ref
    putSide side
    putShares shares
    putStock stock
    putPrice price
    putMatchNumber match

putTag :: Char -> Put
putTag = putWord8 . fromIntegral . fromEnum

putHeader :: Header -> Put
putHeader (Header (StockLocate sl) (TrackingNumber tn) ts) = do
  putWord16be sl
  putWord16be tn
  putWord48be (timestampNanos ts)

putOrderRef :: OrderRef -> Put
putOrderRef (OrderRef r) = putWord64be r

putMatchNumber :: MatchNumber -> Put
putMatchNumber (MatchNumber m) = putWord64be m

putShares :: Shares -> Put
putShares (Shares s) = putWord32be s

putPrice :: Price -> Put
putPrice (Price p) = putWord32be p

putStock :: Stock -> Put
putStock = putByteString . stockBytes

putSide :: Side -> Put
putSide Buy = putWord8 0x42
putSide Sell = putWord8 0x53

putSystemEvent :: SystemEventCode -> Put
putSystemEvent code = putWord8 $ case code of
  StartOfMessages -> 0x4F
  StartOfSystemHours -> 0x53
  StartOfMarketHours -> 0x51
  EndOfMarketHours -> 0x4D
  EndOfSystemHours -> 0x45
  EndOfMessages -> 0x43

putPrintable :: Printable -> Put
putPrintable IsPrintable = putWord8 0x59
putPrintable NotPrintable = putWord8 0x4E

-- ---------------------------------------------------------------------------
-- 48-bit big-endian helpers (ITCH timestamps are 6 bytes)
-- ---------------------------------------------------------------------------

getWord48be :: Get Word64
getWord48be = do
  hi <- getWord16be
  lo <- getWord32be
  pure ((fromIntegral hi `shiftL` 32) .|. fromIntegral lo)

putWord48be :: Word64 -> Put
putWord48be w = do
  putWord16be (fromIntegral (w `shiftR` 32))
  putWord32be (fromIntegral (w .&. 0xFFFFFFFF))
