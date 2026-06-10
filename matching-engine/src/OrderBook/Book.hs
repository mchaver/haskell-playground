{-# LANGUAGE DerivingStrategies #-}

-- | The order book and the matching engine.
--
-- The book is two price-indexed maps (bids and asks). Each price level holds a
-- FIFO queue of resting orders, so iteration order within a level is arrival
-- order. This determines time priority. Across levels we always walk
-- the best price first (highest bid/lowest ask), which is price priority.
--
-- 'match' is the single entry point: it takes an incoming order and a book and
-- returns the resulting trades, the taker's outcome, and the new book.
module OrderBook.Book
  ( Book
  , emptyBook
  , bids
  , asks
  , bestBid
  , bestAsk
  , restingOrders
  , bookQuantity
  , MatchResult (..)
  , match
  ) where

import           Data.Foldable   (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq, ViewL (..), viewl, (<|))
import qualified Data.Sequence   as Seq

import           OrderBook.Types

-- | A central limit order book. Each side maps a price to the FIFO queue of
-- orders resting at that price (front of the queue = earliest arrival).
data Book = Book
  { bkBids :: Map Price (Seq Resting)
  , bkAsks :: Map Price (Seq Resting)
  , bkSeq  :: Word  -- ^ next arrival sequence number to hand out
  }
  deriving stock (Eq, Show)

emptyBook :: Book
emptyBook = Book Map.empty Map.empty 0

-- | Bid levels, best (highest) price first.
bids :: Book -> [(Price, [Resting])]
bids = map (fmap toList) . Map.toDescList . bkBids

-- | Ask levels, best (lowest) price first.
asks :: Book -> [(Price, [Resting])]
asks = map (fmap toList) . Map.toAscList . bkAsks

bestBid :: Book -> Maybe Price
bestBid = fmap (fst . fst) . Map.maxViewWithKey . bkBids

bestAsk :: Book -> Maybe Price
bestAsk = fmap (fst . fst) . Map.minViewWithKey . bkAsks

-- | Every resting order on the book, in no particular order. Handy for tests.
restingOrders :: Book -> [Resting]
restingOrders bk =
  concatMap (toList . snd) (Map.toList (bkBids bk))
    ++ concatMap (toList . snd) (Map.toList (bkAsks bk))

-- | Total resting quantity across both sides.
bookQuantity :: Book -> Int
bookQuantity = sum . map (quantityInt . rQty) . restingOrders

-- | The result of matching one incoming order against the book.
data MatchResult = MatchResult
  { mrTrades :: [Trade]
  , mrStatus :: TakerStatus
  , mrBook   :: Book
  }
  deriving stock (Eq, Show)

-- | Match an incoming order against the book.
--
-- Fill-or-kill is handled by running the match speculatively and only
-- committing it if the order filled completely; otherwise the original book is
-- returned untouched and the taker is 'Rejected'.
match :: NewOrder -> Book -> MatchResult
match order book =
  case noTif order of
    FOK ->
      let result = execute order book
      in if mrStatus result == FullyFilled
           then result
           else MatchResult [] Rejected book
    _ -> execute order book

-- | Walk the opposite side, generate trades, then decide what happens to any
-- unfilled remainder according to order type and time-in-force.
execute :: NewOrder -> Book -> MatchResult
execute order book =
  let side                     = noSide order
      want                     = quantityInt (noQty order)
      (trades, filled, oppMap) = consume side (noId order) (noType order) want (oppositeMap side book)
      leftover                 = want - filled
      bookAfter                = setOppositeMap side oppMap book
  in finalize order leftover trades bookAfter

-- | Pick the side of the book the taker trades against.
oppositeMap :: Side -> Book -> Map Price (Seq Resting)
oppositeMap Buy  = bkAsks
oppositeMap Sell = bkBids

setOppositeMap :: Side -> Map Price (Seq Resting) -> Book -> Book
setOppositeMap Buy  m bk = bk { bkAsks = m }
setOppositeMap Sell m bk = bk { bkBids = m }

-- | Consume marketable liquidity from the opposite side.
--
-- Returns the trades produced, the total quantity filled, and the opposite-side
-- map with consumed orders removed (or reduced). Stops at the first level that
-- is not marketable, or when the taker's quantity is exhausted.
consume
  :: Side
  -> OrderId
  -> OrderType
  -> Int                          -- ^ remaining taker quantity
  -> Map Price (Seq Resting)      -- ^ opposite side
  -> ([Trade], Int, Map Price (Seq Resting))
consume side taker otype = go
  where
    -- The best level for the taker to hit: lowest ask (buyer) / highest bid (seller).
    bestView m = case side of
      Buy  -> Map.minViewWithKey m
      Sell -> Map.maxViewWithKey m

    -- Is a level at this price marketable for the taker?
    marketable lvlPrice = case otype of
      Market    -> True
      Limit lim -> case side of
        Buy  -> lim >= lvlPrice
        Sell -> lim <= lvlPrice

    go remaining m
      | remaining <= 0 = ([], 0, m)
      | otherwise =
          case bestView m of
            Nothing -> ([], 0, m)
            Just ((lvlPrice, queue), restMap)
              | not (marketable lvlPrice) -> ([], 0, m)
              | otherwise ->
                  let (lvlTrades, lvlFilled, queue', remaining') =
                        matchLevel taker lvlPrice remaining queue
                      m' = if Seq.null queue'
                             then restMap
                             else Map.insert lvlPrice queue' restMap
                      (more, moreFilled, m'') = go remaining' m'
                  in (lvlTrades ++ more, lvlFilled + moreFilled, m'')

-- | Match the taker against a single price level's FIFO queue.
--
-- Walks the queue front-to-back (time priority). Returns the trades, the
-- quantity filled at this level, the queue with consumed orders removed, and
-- the taker quantity still outstanding after this level.
matchLevel
  :: OrderId
  -> Price
  -> Int
  -> Seq Resting
  -> ([Trade], Int, Seq Resting, Int)
matchLevel taker lvlPrice = walk
  where
    walk remaining queue
      | remaining <= 0 = ([], 0, queue, remaining)
      | otherwise =
          case viewl queue of
            EmptyL -> ([], 0, queue, remaining)
            maker :< rest ->
              let avail = quantityInt (rQty maker)
                  tq    = min remaining avail
                  trade = Trade lvlPrice (unsafeQty tq) taker (rId maker)
              in if tq == avail
                   then -- Maker fully consumed: pop it and continue down the queue.
                     let (more, filled, queue', remaining') = walk (remaining - tq) rest
                     in (trade : more, tq + filled, queue', remaining')
                   else -- Maker partially consumed: taker is exhausted, maker stays.
                     let maker' = maker { rQty = unsafeQty (avail - tq) }
                     in ([trade], tq, maker' <| rest, 0)

-- | Decide the fate of the unfilled remainder.
finalize :: NewOrder -> Int -> [Trade] -> Book -> MatchResult
finalize order leftover trades bookAfter
  | leftover <= 0 = MatchResult trades FullyFilled bookAfter
  | otherwise =
      case (noType order, noTif order) of
        -- GTC limit orders rest their remainder on the book.
        (Limit p, GTC) ->
          let resting = Resting
                { rId    = noId order
                , rSide  = noSide order
                , rPrice = p
                , rQty   = unsafeQty leftover
                , rSeq   = SequenceNumber (bkSeq bookAfter)
                }
              bookRested = insertResting resting bookAfter { bkSeq = bkSeq bookAfter + 1 }
          in MatchResult trades (restingStatus trades) bookRested
        -- Everything else (Market, IOC, and the FOK speculative path) cancels
        -- the remainder. FOK's "all-or-nothing" is enforced in 'match'.
        _ -> MatchResult trades (cancelledStatus trades) bookAfter
  where
    restingStatus ts   = if null ts then NoFillResting   else PartiallyFilledResting
    cancelledStatus ts = if null ts then NoFillCancelled else PartiallyFilledCancelled

-- | Append a resting order to the back of its price level's queue (time priority).
insertResting :: Resting -> Book -> Book
insertResting r bk = case rSide r of
  Buy  -> bk { bkBids = Map.insertWith (flip (Seq.><)) (rPrice r) (Seq.singleton r) (bkBids bk) }
  Sell -> bk { bkAsks = Map.insertWith (flip (Seq.><)) (rPrice r) (Seq.singleton r) (bkAsks bk) }

-- | Build a 'Quantity' from a value the engine has already proven positive.
-- A failure here means a matching invariant was violated, which is a bug.
unsafeQty :: Int -> Quantity
unsafeQty n = case mkQuantity n of
  Just q  -> q
  Nothing -> error ("matchLevel: non-positive quantity " ++ show n ++ " (invariant violated)")
