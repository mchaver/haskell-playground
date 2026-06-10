{-# LANGUAGE OverloadedStrings #-}

-- | A scripted tour of the codec: build a few ITCH messages, show their wire
-- bytes, decode them back, and then feed the decoder several kinds of
-- malformed input to show every failure is a typed value, not an exception.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.Word (Word16, Word64, Word8)
import Numeric (showHex)

import ITCH.Codec (decode, encode)
import ITCH.Types
  ( Header (..)
  , MatchNumber (..)
  , Message (..)
  , OrderRef (..)
  , Price (..)
  , Shares (..)
  , Side (..)
  , StockLocate (..)
  , SystemEventCode (..)
  , TrackingNumber (..)
  , mkStock
  , mkTimestamp
  )

-- | Build a header, failing (to 'Nothing') if the timestamp is out of range.
mkHeader :: Word16 -> Word16 -> Word64 -> Maybe Header
mkHeader sl tn ts =
  Header (StockLocate sl) (TrackingNumber tn) <$> mkTimestamp ts

-- | A handful of sample messages. Each is built in 'Maybe' because the ticker
-- and timestamp go through invariant-checking smart constructors.
samples :: [Message]
samples =
  catMaybes
    [ do
        h <- mkHeader 0 0 1_000
        pure (SystemEvent h StartOfMessages)
    , do
        h <- mkHeader 42 1 39_755_123_456
        stk <- mkStock "AAPL    "
        pure (AddOrder h (OrderRef 1001) Buy (Shares 100) stk (Price 1_505_000))
    , do
        h <- mkHeader 42 2 39_755_200_000
        stk <- mkStock "AAPL    "
        pure (Trade h (OrderRef 1001) Sell (Shares 50) stk (Price 1_505_000) (MatchNumber 7))
    , do
        h <- mkHeader 42 3 39_755_900_000
        pure (OrderDelete h (OrderRef 1001))
    ]

hexDump :: ByteString -> String
hexDump = unwords . map byte . BS.unpack
 where
  byte w =
    let s = showHex w ""
     in if length s == 1 then '0' : s else s

-- | Replace the byte at an index (used to corrupt one field in a valid frame).
patchByte :: Int -> Word8 -> ByteString -> ByteString
patchByte i b bs = BS.take i bs <> BS.singleton b <> BS.drop (i + 1) bs

showRoundTrip :: Message -> IO ()
showRoundTrip msg = do
  let bs = encode msg
  putStrLn ("  " <> show msg)
  putStrLn ("    bytes (" <> show (BS.length bs) <> "): " <> hexDump bs)
  putStrLn ("    decode: " <> show (decode bs))
  putStrLn ("    round-trips: " <> show (decode bs == Right msg))

showFailure :: String -> ByteString -> IO ()
showFailure label bs =
  putStrLn ("  " <> label <> "\n    -> " <> show (decode bs))

main :: IO ()
main = do
  putStrLn "== encode / decode round-trip =="
  mapM_ showRoundTrip samples

  putStrLn "\n== malformed input (every failure is a typed DecodeError) =="
  let addOrderBytes = case samples of
        (_ : a : _) -> encode a
        _ -> BS.empty
  showFailure "empty input" BS.empty
  showFailure "unknown message type 0x5A ('Z')" (BS.pack [0x5A, 0x00, 0x00])
  showFailure
    "truncated Add Order (first 20 of 36 bytes)"
    (BS.take 20 addOrderBytes)
  showFailure
    "Add Order with 3 trailing bytes"
    (addOrderBytes <> BS.pack [0xDE, 0xAD, 0xBE])
  showFailure
    "Add Order with an invalid Buy/Sell byte"
    (patchByte 19 0x00 addOrderBytes)
