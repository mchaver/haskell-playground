{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.Word (Word8)

import Hedgehog
  ( Gen
  , Group (..)
  , Property
  , annotateShow
  , checkParallel
  , failure
  , forAll
  , property
  , success
  , tripping
  , (===)
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main (defaultMain)
import Hedgehog.Range qualified as Range

import ITCH.Codec (decode, encode, knownMessageTypes)
import ITCH.Types
  ( DecodeError (..)
  , Header (..)
  , MatchNumber (..)
  , Message (..)
  , OrderRef (..)
  , Price (..)
  , Printable
  , Shares (..)
  , Side
  , Stock
  , StockLocate (..)
  , SystemEventCode
  , TrackingNumber (..)
  , maxTimestamp
  , mkStock
  , mkTimestamp
  )

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genStockLocate :: Gen StockLocate
genStockLocate = StockLocate <$> Gen.word16 Range.constantBounded

genTrackingNumber :: Gen TrackingNumber
genTrackingNumber = TrackingNumber <$> Gen.word16 Range.constantBounded

genOrderRef :: Gen OrderRef
genOrderRef = OrderRef <$> Gen.word64 Range.constantBounded

genMatchNumber :: Gen MatchNumber
genMatchNumber = MatchNumber <$> Gen.word64 Range.constantBounded

genShares :: Gen Shares
genShares = Shares <$> Gen.word32 Range.constantBounded

genPrice :: Gen Price
genPrice = Price <$> Gen.word32 Range.constantBounded

genSide :: Gen Side
genSide = Gen.enumBounded

genSystemEvent :: Gen SystemEventCode
genSystemEvent = Gen.enumBounded

genPrintable :: Gen Printable
genPrintable = Gen.enumBounded

-- | Header timestamps are constrained to the 48-bit wire range.
genHeader :: Gen Header
genHeader =
  Header
    <$> genStockLocate
    <*> genTrackingNumber
    <*> Gen.mapMaybe mkTimestamp (Gen.word64 (Range.linear 0 maxTimestamp))

-- | A valid 8-byte ticker: uppercase letters padded with trailing spaces.
genStock :: Gen Stock
genStock = Gen.mapMaybe mkStock (BS.pack <$> genStockField)
 where
  genStockField = do
    len <- Gen.int (Range.linear 1 8)
    chars <- Gen.list (Range.singleton len) (Gen.word8 (Range.constant 0x41 0x5A))
    pure (take 8 (chars <> replicate 8 0x20))

genMessage :: Gen Message
genMessage =
  Gen.choice
    [ SystemEvent <$> genHeader <*> genSystemEvent
    , AddOrder <$> genHeader <*> genOrderRef <*> genSide <*> genShares <*> genStock <*> genPrice
    , OrderExecuted <$> genHeader <*> genOrderRef <*> genShares <*> genMatchNumber
    , OrderExecutedWithPrice
        <$> genHeader
        <*> genOrderRef
        <*> genShares
        <*> genMatchNumber
        <*> genPrintable
        <*> genPrice
    , OrderCancel <$> genHeader <*> genOrderRef <*> genShares
    , OrderDelete <$> genHeader <*> genOrderRef
    , OrderReplace <$> genHeader <*> genOrderRef <*> genOrderRef <*> genShares <*> genPrice
    , Trade <$> genHeader <*> genOrderRef <*> genSide <*> genShares <*> genStock <*> genPrice <*> genMatchNumber
    ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The headline guarantee: @decode . encode == Right@ for every message.
prop_roundtrip :: Property
prop_roundtrip = property $ do
  m <- forAll genMessage
  tripping m encode decode

-- | Dropping any non-zero suffix of a frame is reported as 'Truncated' — never
-- a silent partial parse, never an exception.
prop_truncated :: Property
prop_truncated = property $ do
  m <- forAll genMessage
  let bs = encode m
  k <- forAll (Gen.int (Range.linear 1 (BS.length bs - 1)))
  case decode (BS.take (BS.length bs - k) bs) of
    Left Truncated {} -> success
    other -> annotateShow other >> failure

-- | Extra bytes after a complete fixed-width message are reported as
-- 'TrailingBytes', with the exact surplus count.
prop_trailing :: Property
prop_trailing = property $ do
  m <- forAll genMessage
  extra <- forAll (Gen.bytes (Range.linear 1 16))
  case decode (encode m <> extra) of
    Left (TrailingBytes n) -> n === BS.length extra
    other -> annotateShow other >> failure

-- | Any leading byte that is not a known message type yields
-- 'UnknownMessageType' carrying that byte, whatever follows it.
prop_unknownType :: Property
prop_unknownType = property $ do
  let known = map (fromIntegral . fromEnum) knownMessageTypes :: [Word8]
  tag <- forAll (Gen.filter (`notElem` known) (Gen.word8 Range.constantBounded))
  rest <- forAll (Gen.bytes (Range.linear 0 48))
  case decode (BS.cons tag rest) of
    Left (UnknownMessageType b) -> b === tag
    other -> annotateShow other >> failure

-- | Empty input is its own typed failure.
prop_empty :: Property
prop_empty = property $ decode BS.empty === Left EmptyInput

main :: IO ()
main =
  defaultMain
    [ checkParallel $
        Group
          "ITCH.Codec"
          [ ("round-trip decode . encode", prop_roundtrip)
          , ("truncated frame -> Truncated", prop_truncated)
          , ("trailing bytes -> TrailingBytes", prop_trailing)
          , ("unknown type -> UnknownMessageType", prop_unknownType)
          , ("empty input -> EmptyInput", prop_empty)
          ]
    ]
