{-# LANGUAGE LinearTypes #-}

-- | Accounts that own 'Money', and the scoped bracket for working with them.
--
-- An 'Account' holds a linear 'Money' balance, so an 'Account' is itself a
-- linear resource: handed one inside a continuation, you must give one back.
-- You cannot quietly drop it (the balance would vanish) and you cannot copy it
-- (the balance would double). 'openAccount' \/ 'closeAccount' bracket that
-- lifetime, and 'transfer' moves value between two accounts while conserving
-- the total.
module Accounting.Ledger
  ( -- * Accounts
    AccountId
  , Account
  , openAccount
  , closeAccount
  , balanceOf

    -- * Moving value
  , deposit
  , transfer

    -- * Scoped use
  , withAccount
  ) where

import Accounting.Money (Amount, Money, amount, burn, merge, mint, split)
import Data.Unrestricted.Linear (Ur (Ur), move)
import Data.Word (Word64)
import Prelude

-- | An opaque handle identifying an account. Just a number; the interesting
-- linearity lives in the balance it is paired with.
type AccountId = Word64

-- | An account: an identifier together with the 'Money' it currently owns.
-- Because the balance is linear, the whole 'Account' is linear — it cannot be
-- duplicated or dropped, only handed on.
data Account = Account !AccountId !Money

-- | Open an account funded with @a@ units brought in from outside (this calls
-- the trusted 'Accounting.Money.mint' boundary under the hood).
openAccount :: AccountId -> Amount -> Account
openAccount i a = Account i (mint a)

-- | Close an account, withdrawing whatever balance remains back to the outside
-- world as a plain, copyable 'Amount'. This consumes the account — after this
-- there is no handle left to misuse.
closeAccount :: Account %1 -> Ur Amount
closeAccount (Account i bal) = case move i of
  Ur _ -> burn bal

-- | Read an account's balance without closing it: the account is threaded back
-- out alongside a copyable snapshot of its balance.
balanceOf :: Account %1 -> (Ur Amount, Account)
balanceOf (Account i bal) = case move i of
  Ur i' -> case amount bal of
    (amt, bal') -> (amt, Account i' bal')

-- | Pour a pile of 'Money' into an account, consuming both and conserving the
-- total.
deposit :: Money %1 -> Account %1 -> Account
deposit m (Account i bal) = case move i of
  Ur i' -> Account i' (merge m bal)

-- | Move @n@ units from the first account to the second, conserving the grand
-- total. The 'Ur' 'Bool' reports whether the transfer happened; if the source
-- lacked the funds both accounts come back untouched and it is 'False'.
transfer :: Amount -> Account %1 -> Account %1 -> (Account, Account, Ur Bool)
transfer n (Account si sbal) dst = case move si of
  Ur si' -> case split n sbal of
    Left whole -> (Account si' whole, dst, Ur False)
    Right (moved, rest) -> (Account si' rest, deposit moved dst, Ur True)

-- | Run a continuation with a freshly opened, funded account and settle it
-- afterwards. The continuation is handed a linear 'Account' and must return
-- one (it cannot leak or drop it); whatever balance is left is then withdrawn
-- and returned. This is the resource-safety face of linear types: the bracket
-- makes \"forgot to settle the account\" a type error.
withAccount :: AccountId -> Amount -> (Account %1 -> Account) %1 -> Ur Amount
withAccount i a k = closeAccount (k (openAccount i a))
