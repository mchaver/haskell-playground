{-# LANGUAGE DerivingStrategies #-}

-- | Core domain types for the matching engine.
--
--   * 'Quantity' is strictly positive by construction.
--   * A 'Resting' order always has a limit 'Price'. Market orders never rest.
--   * Order arrival is captured by 'SequenceNumber', which is the basis for
--     time priority within a price level.

module OrderBook.Types
  ( Side (..)
  , opposite
  , Price (..)
  , Quantity
  , mkQuantity
  , quantityInt
  , minQuantity
  , subQuantity
  , OrderId (..)
  , SequenceNumber (..)
  , TimeInForce (..)
  , OrderType (..)
  , NewOrder (..)
  , Resting (..)
  , Trade (..)
  , TakerStatus (..)
  ) where

-- | The side of an order.
data Side
  = Buy
  | Sell
  deriving stock (Eq, Ord, Show, Enum, Bounded)

opposite :: Side -> Side
opposite Buy  = Sell
opposite Sell = Buy

-- | Price in integer ticks. 
newtype Price
  = Price Int
  deriving stock (Eq, Ord, Show)

-- | A strictly-positive quantity. Do not export constructor.
newtype Quantity = Quantity Int
  deriving stock (Eq, Ord, Show)

-- | Create only positive quantities.
mkQuantity :: Int -> Maybe Quantity
mkQuantity n
  | n > 0     = Just (Quantity n)
  | otherwise = Nothing

quantityInt :: Quantity -> Int
quantityInt (Quantity n) = n

-- | The smaller of two quantities. Total: the minimum of two strictly-positive
-- values is itself strictly positive, so no smart constructor is needed.
minQuantity :: Quantity -> Quantity -> Quantity
minQuantity (Quantity a) (Quantity b) = Quantity (min a b)

-- | Subtract the second quantity from the first. Returns 'Nothing' when the
-- result would not be strictly positive (i.e. the second is greater than or
-- equal to the first), preserving the positivity invariant without 'error'.
subQuantity :: Quantity -> Quantity -> Maybe Quantity
subQuantity (Quantity a) (Quantity b)
  | a > b     = Just (Quantity (a - b))
  | otherwise = Nothing

-- | Stable identifier for an order, assigned by the gateway/client.
newtype OrderId
  = OrderId Int
  deriving stock (Eq, Ord, Show)

-- | Monotonic arrival sequence number. Lower means earlier, which gives time
-- priority within a price level.
newtype SequenceNumber = SequenceNumber Word
  deriving stock (Eq, Ord, Show)

-- | Time-in-force policy for an incoming order.
data TimeInForce
  = GTC  -- ^ Good-til-cancelled: rest any unfilled remainder on the book.
  | IOC  -- ^ Immediate-or-cancel: fill what is marketable now, cancel the rest.
  | FOK  -- ^ Fill-or-kill: fill in full immediately, otherwise do nothing.
  deriving stock (Eq, Show)

-- | A 'Limit' order only trades at its price or better.
-- a 'Market' order trades at any price and never rests.
data OrderType
  = Limit Price
  | Market
  deriving stock (Eq, Show)

-- | An incoming order, before it has interacted with the book.
data NewOrder = NewOrder
  { noId   :: OrderId
  , noSide :: Side
  , noType :: OrderType
  , noTif  :: TimeInForce
  , noQty  :: Quantity
  }
  deriving stock (Eq, Show)

-- | An order resting on the book. By construction it always carries a limit
-- price and a positive remaining quantity.
data Resting = Resting
  { rId    :: OrderId
  , rSide  :: Side
  , rPrice :: Price
  , rQty   :: Quantity
  , rSeq   :: SequenceNumber
  }
  deriving stock (Eq, Show)

-- | An executed trade between a taker (the incoming order) and a maker (a
-- resting order). The price is always the maker's resting price.
data Trade = Trade
  { trPrice :: Price
  , trQty   :: Quantity
  , trTaker :: OrderId
  , trMaker :: OrderId
  }
  deriving stock (Eq, Show)

-- | The outcome for the incoming (taker) order.
data TakerStatus
  = FullyFilled               -- ^ The whole order traded.
  | PartiallyFilledResting    -- ^ Some traded, the remainder is now resting.
  | PartiallyFilledCancelled  -- ^ Some traded, the remainder was cancelled (IOC/Market).
  | NoFillResting             -- ^ Nothing traded, the whole order is now resting.
  | NoFillCancelled           -- ^ Nothing traded, nothing rested (IOC/Market, no liquidity).
  | Rejected                  -- ^ FOK could not fill in full, the book is untouched.
  deriving stock (Eq, Show)
