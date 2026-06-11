{-# LANGUAGE LinearTypes #-}

-- | 'Money' as a /linear resource/.
--
-- The point of this module is to make a conservation law a compile-time
-- guarantee. A value of type 'Money' must be used /exactly once/:
--
--   * it cannot be duplicated — there is no 'Dupable' instance, so you cannot
--     turn one pile of cash into two (no counterfeiting); and
--   * it cannot be dropped — there is no 'Consumable' instance, so a 'Money'
--     value can never simply go out of scope (no funds silently vanishing).
--
-- Every operation below conserves the total: 'split' and 'merge' only move
-- value around. The single trusted boundary is 'mint' (funds enter the system
-- from outside) and 'burn' (funds leave it). Those two are the only functions
-- that change how much money exists, so they are the only places to audit.
module Accounting.Money
  ( -- * The resource
    Amount
  , Money

    -- * Trusted boundary (create or destroy value)
  , mint
  , burn

    -- * Conservation preserving operations
  , zero
  , amount
  , split
  , merge
  ) where

import Data.Unrestricted.Linear (Ur (Ur), move)
import Data.Word (Word64)
import Prelude

-- | A magnitude of funds, in the smallest indivisible unit (think cents). This
-- is an ordinary, unrestricted number; it is the /tagged/ 'Money' wrapper that
-- carries the linear obligation.
type Amount = Word64

-- | A quantity of funds. The constructor is hidden: outside this module the
-- only way to obtain a 'Money' is through 'mint', and the only way to discard
-- one is through 'burn'. Note the deliberate /absence/ of 'Consumable' and
-- 'Dupable' instances — that absence is the whole guarantee (a hidden
-- constructor also blocks @coerce@ from forging one).
newtype Money = Money Amount

-- | Bring funds into the system from the outside world (a deposit, a mint, the
-- opening float of a till). This is one half of the trusted boundary: it
-- conjures a linear 'Money' from a plain 'Amount', so it is exactly where new
-- value enters and the place to audit for inflation.
mint :: Amount -> Money
mint = Money

-- | Send funds out of the system (a withdrawal, a payout). The 'Money' is
-- consumed and its magnitude handed back as an 'Ur' (unrestricted) 'Amount'
-- that you are free to copy and print. This is the other half of the trusted
-- boundary: the only sanctioned way to make a 'Money' value disappear.
burn :: Money %1 -> Ur Amount
burn (Money a) = move a

-- | An empty amount of money. No restrictions required.
zero :: Money
zero = Money 0

-- | Read the magnitude of an amount of money /without/ destroying it. 'Money' is
-- linear we cannot simply look at it: we consume the input and hand back both the
-- 'Amount' (as a copyable 'Ur') and a fresh 'Money' of equal value, so the
-- total is preserved.
amount :: Money %1 -> (Ur Amount, Money)
amount (Money a) = case move a of
  Ur x -> (Ur x, Money x)

-- | Split @n@ off the front of a pile, conserving the total: the two results
-- sum back to the input. If the pile holds less than @n@ there is no way to
-- conserve value, so the original pile is returned untouched in 'Left'.
--
-- prop> split n m == Right (a, b)  ==>  value a + value b == value m
-- prop> split n m == Left m'       ==>  value m'          == value m
split :: Amount -> Money %1 -> Either Money (Money, Money)
split n (Money a) = case move a of
  Ur x
    | n <= x -> Right (Money n, Money (x - n))
    | otherwise -> Left (Money x)

-- | Combine two amount of Money into one, conserving the total.
-- Both inputs are consumed (each exactly once) and their magnitudes added.
merge :: Money %1 -> Money %1 -> Money
merge (Money a) (Money b) = case move a of
  Ur x -> case move b of
    Ur y -> Money (x + y)
