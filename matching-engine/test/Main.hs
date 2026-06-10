{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.List          (foldl')
import           Data.Maybe         (fromMaybe)

import           Hedgehog
import qualified Hedgehog.Gen       as Gen
import           Hedgehog.Main      (defaultMain)
import qualified Hedgehog.Range     as Range

import           OrderBook.Book
import           OrderBook.Types

-- ---------------------------------------------------------------------------
-- Generators
--
-- Prices and quantities are kept in small ranges on purpose, so that generated
-- orders frequently cross and we actually exercise the matching paths rather
-- than building two disjoint stacks that never trade.
-- ---------------------------------------------------------------------------

genPrice :: Gen Price
genPrice = Price <$> Gen.int (Range.linearFrom 100 90 110)

genQty :: Gen Quantity
genQty = fromMaybe (error "genQty") . mkQuantity <$> Gen.int (Range.linear 1 8)

genSide :: Gen Side
genSide = Gen.element [Buy, Sell]

genTif :: Gen TimeInForce
genTif = Gen.element [GTC, IOC, FOK]

genType :: Gen OrderType
genType = Gen.frequency [(4, Limit <$> genPrice), (1, pure Market)]

-- | An order spec without an id; ids are assigned positionally when a flow is
-- replayed, which keeps them unique and stable.
genSpec :: Gen NewOrder
genSpec =
  NewOrder (OrderId (-1)) <$> genSide <*> genType <*> genTif <*> genQty

-- | A spec that is guaranteed to rest if unmatched: GTC limit only.
genRestingSpec :: Gen NewOrder
genRestingSpec =
  NewOrder (OrderId (-1)) <$> genSide <*> (Limit <$> genPrice) <*> pure GTC <*> genQty

assignIds :: [NewOrder] -> [NewOrder]
assignIds = zipWith (\i o -> o { noId = OrderId i }) [0 ..]

-- | Replay a flow of specs into a book, threading the resulting book through.
buildBook :: [NewOrder] -> Book
buildBook = foldl' (\bk o -> mrBook (match o bk)) emptyBook . assignIds

tradedQty :: [Trade] -> Int
tradedQty = sum . map (quantityInt . trQty)

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Quantity is conserved. Every unit the makers gave up is a unit the taker
-- received, and the taker's remainder either rests on the book or is cancelled.
--
--   bookBefore - bookAfter == traded - restedByTaker
--
-- i.e. the book shrinks by exactly the liquidity consumed, less whatever the
-- taker added back by resting.
prop_conservation :: Property
prop_conservation = property $ do
  restSpecs <- forAll (Gen.list (Range.linear 0 40) genRestingSpec)
  inSpec    <- forAll genSpec
  let book     = buildBook restSpecs
      incoming = inSpec { noId = OrderId 100000 }
      res      = match incoming book
      traded   = tradedQty (mrTrades res)
      want     = quantityInt (noQty incoming)
      rested   = case mrStatus res of
                   PartiallyFilledResting -> want - traded
                   NoFillResting          -> want
                   _                      -> 0
  (bookQuantity book - bookQuantity (mrBook res)) === (traded - rested)

-- | The book is never crossed: after any flow, the best bid is strictly below
-- the best ask. A crossed book would mean a trade was left on the table.
prop_neverCrossed :: Property
prop_neverCrossed = property $ do
  specs <- forAll (Gen.list (Range.linear 0 80) genSpec)
  -- Check the invariant after *every* prefix, not just the end state.
  let books = scanl (\bk o -> mrBook (match o bk)) emptyBook (assignIds specs)
  mapM_ assertUncrossed books
  where
    assertUncrossed bk =
      case (bestBid bk, bestAsk bk) of
        (Just b, Just a) -> diff b (<) a
        _                -> success

-- | Price protection: a trade never executes outside the taker's limit.
-- A buyer never pays more than its limit; a seller never sells below it.
prop_priceProtection :: Property
prop_priceProtection = property $ do
  restSpecs <- forAll (Gen.list (Range.linear 0 40) genRestingSpec)
  inSpec    <- forAll genSpec
  let book     = buildBook restSpecs
      incoming = inSpec { noId = OrderId 100000 }
      res      = match incoming book
  mapM_ (assertWithinLimit incoming) (mrTrades res)
  where
    assertWithinLimit o t =
      case noType o of
        Market    -> success
        Limit lim -> case noSide o of
          Buy  -> diff (trPrice t) (<=) lim
          Sell -> diff (trPrice t) (>=) lim

-- | Price priority: a taker's fills improve monotonically away from it.
-- For a buyer the trade prices are non-decreasing (cheapest asks first); for a
-- seller they are non-increasing (richest bids first).
prop_pricePriority :: Property
prop_pricePriority = property $ do
  restSpecs <- forAll (Gen.list (Range.linear 0 40) genRestingSpec)
  inSpec    <- forAll genSpec
  let book      = buildBook restSpecs
      incoming  = inSpec { noId = OrderId 100000 }
      res       = match incoming book
      prices    = map trPrice (mrTrades res)
      ordered   = case noSide incoming of
                    Buy  -> nonDecreasing prices
                    Sell -> nonIncreasing prices
  assert ordered
  where
    nonDecreasing xs = and (zipWith (<=) xs (drop 1 xs))
    nonIncreasing xs = and (zipWith (>=) xs (drop 1 xs))

-- | Fill-or-kill is atomic. A FOK order is either filled in full (and the trade
-- quantity equals the order quantity) or it is rejected with no trades and the
-- book is left exactly as it was. No other outcome is possible.
prop_fokAtomic :: Property
prop_fokAtomic = property $ do
  restSpecs <- forAll (Gen.list (Range.linear 0 40) genRestingSpec)
  inSpec    <- forAll genSpec
  let book     = buildBook restSpecs
      incoming = inSpec { noId = OrderId 100000, noTif = FOK }
      res      = match incoming book
  case mrStatus res of
    FullyFilled -> tradedQty (mrTrades res) === quantityInt (noQty incoming)
    Rejected    -> do
      mrTrades res === []
      mrBook res   === book
    other -> do
      footnote ("FOK produced an unexpected status: " ++ show other)
      failure

-- | Every resting order always carries a positive quantity. (The type makes a
-- non-positive 'Quantity' unconstructable; this guards that the engine never
-- leaves a fully-filled maker on the book by mistake.)
prop_noZeroResting :: Property
prop_noZeroResting = property $ do
  specs <- forAll (Gen.list (Range.linear 0 80) genSpec)
  let book = buildBook specs
  assert (all ((> 0) . quantityInt . rQty) (restingOrders book))

main :: IO ()
main =
  defaultMain
    [ checkParallel $ Group "OrderBook"
        [ ("conservation of quantity",  prop_conservation)
        , ("book is never crossed",     prop_neverCrossed)
        , ("taker price protection",    prop_priceProtection)
        , ("price priority of fills",   prop_pricePriority)
        , ("fill-or-kill is atomic",    prop_fokAtomic)
        , ("no zero-qty resting order", prop_noZeroResting)
        ]
    ]
