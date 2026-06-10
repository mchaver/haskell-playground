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

import           OrderBook.Types (NewOrder (..), OrderId, OrderType (..),
                                  Price, Quantity, Resting (..),
                                  SequenceNumber (SequenceNumber), Side (..),
                                  TakerStatus (..), TimeInForce (..),
                                  Trade (..), minQuantity, quantityInt,
                                  subQuantity)

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
  let side                       = noSide order
      (trades, leftover, oppMap) = consume side (noId order) (noType order) (noQty order) (oppositeMap side book)
      bookAfter                  = setOppositeMap side oppMap book
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
-- Returns the trades produced, the taker's unfilled remainder ('Nothing' once
-- the taker is exhausted), and the opposite-side map with consumed orders
-- removed (or reduced). Stops at the first level that is not marketable, or
-- when the taker's quantity is exhausted.
consume
  :: Side
  -> OrderId
  -> OrderType
  -> Quantity                     -- ^ remaining taker quantity
  -> Map Price (Seq Resting)      -- ^ opposite side
  -> ([Trade], Maybe Quantity, Map Price (Seq Resting))
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

    go remaining m =
      case bestView m of
        Nothing -> ([], Just remaining, m)
        Just ((lvlPrice, queue), restMap)
          | not (marketable lvlPrice) -> ([], Just remaining, m)
          | otherwise ->
              let (lvlTrades, queue', remaining') = matchLevel taker lvlPrice remaining queue
                  m' = if Seq.null queue'
                         then restMap
                         else Map.insert lvlPrice queue' restMap
              in case remaining' of
                   Nothing   -> (lvlTrades, Nothing, m')
                   Just rem' ->
                     let (more, leftover, m'') = go rem' m'
                     in (lvlTrades ++ more, leftover, m'')

-- | Match the taker against a single price level's FIFO queue.
--
-- Walks the queue front-to-back (time priority). Returns the trades, the queue
-- with consumed orders removed, and the taker quantity still outstanding after
-- this level ('Nothing' once the taker is exhausted). All quantities stay in
-- the strictly-positive 'Quantity' domain, so no partial reconstruction from a
-- raw 'Int' is ever needed.
matchLevel
  :: OrderId
  -> Price
  -> Quantity
  -> Seq Resting
  -> ([Trade], Seq Resting, Maybe Quantity)
matchLevel taker lvlPrice = walk
  where
    walk remaining queue =
      case viewl queue of
        EmptyL -> ([], queue, Just remaining)
        maker :< rest ->
          let availQ = rQty maker
              tradeQ = minQuantity remaining availQ
              trade  = Trade lvlPrice tradeQ taker (rId maker)
          in case subQuantity availQ tradeQ of
               -- Maker partially consumed (avail > trade): taker is exhausted,
               -- the maker stays with its reduced remainder.
               Just makerLeft ->
                 ([trade], maker { rQty = makerLeft } <| rest, Nothing)
               -- Maker fully consumed: pop it, then continue only if the taker
               -- still has quantity outstanding.
               Nothing ->
                 case subQuantity remaining tradeQ of
                   Nothing   -> ([trade], rest, Nothing)
                   Just rem' ->
                     let (more, queue', leftover) = walk rem' rest
                     in (trade : more, queue', leftover)

-- | Decide the fate of the unfilled remainder. A 'Nothing' leftover means the
-- taker filled completely; a @'Just' q@ leftover is the quantity still owed.
finalize :: NewOrder -> Maybe Quantity -> [Trade] -> Book -> MatchResult
finalize _ Nothing trades bookAfter = MatchResult trades FullyFilled bookAfter
finalize order (Just leftover) trades bookAfter =
  case (noType order, noTif order) of
    -- GTC limit orders rest their remainder on the book.
    (Limit p, GTC) ->
      let resting = Resting
            { rId    = noId order
            , rSide  = noSide order
            , rPrice = p
            , rQty   = leftover
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
