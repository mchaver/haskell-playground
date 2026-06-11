{-# LANGUAGE LinearTypes #-}

-- | A walk through the linear-accounting API: a couple of valid runs that the
-- compiler accepts, plus a gallery of mistakes it /rejects/. The rejected
-- snippets are kept as comments — uncomment any one of them and the package
-- stops compiling with the error noted alongside.
module Main (main) where

import Accounting.Ledger
  ( closeAccount
  , deposit
  , openAccount
  , transfer
  , withAccount
  )
import Accounting.Money (mint)
import Data.Unrestricted.Linear (Ur (Ur))
import Prelude

main :: IO ()
main = do
  putStrLn "== linear-accounting demo =="

  -- A valid transfer. Open two accounts, move 30 units from #1 to #2, then
  -- settle both. The grand total is conserved: 100 + 0 == 70 + 30.
  let (a, b, Ur ok) = transfer 30 (openAccount 1 100) (openAccount 2 0)
      Ur leftA = closeAccount a
      Ur leftB = closeAccount b
  putStrLn ("transfer 30 succeeded? " <> show ok)
  putStrLn ("account 1 settled with: " <> show leftA)
  putStrLn ("account 2 settled with: " <> show leftB)
  putStrLn ("conserved total:        " <> show (leftA + leftB))

  -- A transfer that can't be funded: #1 only holds 5, so asking for 30 leaves
  -- both accounts untouched and reports False.
  let (c, d, Ur ok2) = transfer 30 (openAccount 1 5) (openAccount 2 0)
      Ur leftC = closeAccount c
      Ur leftD = closeAccount d
  putStrLn ("\noverdraw attempt succeeded? " <> show ok2)
  putStrLn ("account 1 still holds:       " <> show leftC)
  putStrLn ("account 2 still holds:       " <> show leftD)

  -- The scoped bracket: 'withAccount' hands the continuation a *linear*
  -- account that it must consume and hand back. Here the continuation deposits
  -- a 50-unit bonus into it, so the opening 250 settles as 300. The account is
  -- used exactly once — drop it or copy it and this stops compiling.
  let Ur settled = withAccount 7 250 (deposit (mint 50))
  putStrLn ("\nwithAccount 250 + 50 bonus settled with: " <> show settled)

-- ---------------------------------------------------------------------------
-- What the type checker REJECTS. Uncomment to watch a conservation violation
-- become a compile error.
-- ---------------------------------------------------------------------------

-- Each error below is the verbatim GHC 9.6 message; the shared shape is a
-- multiplicity mismatch — a value that must be used once ('One') is used some
-- other number of times ('Many').

-- (1) Dropping a linear account: 'acct' is used 0 times, so its balance would
--     simply vanish.
--
--     • error: [GHC-18872]
--         Couldn't match type 'Many' with 'One'
--           arising from multiplicity of 'acct'
--
-- badDrop :: Account %1 -> Account
-- badDrop acct = openAccount 99 0

-- (2) Duplicating a linear account: 'acct' is used twice, so its balance would
--     double out of nowhere. 'Money' has no 'Dupable' instance, so there is no
--     escape hatch.
--
--     • error: [GHC-18872]
--         Couldn't match type 'Many' with 'One'
--           arising from multiplicity of 'acct'
--
-- badDup :: Account %1 -> (Account, Account)
-- badDup acct = (acct, acct)

-- (3) Leaking funds out of a bracket by ignoring the account handed in:
--     'withAccount' demands an 'Account' back, and '_acct' is used 0 times.
--
--     • error: [GHC-18872]
--         Couldn't match type 'Many' with 'One'
--           arising from multiplicity of '_acct'
--
-- badLeak :: Ur Amount
-- badLeak = withAccount 1 100 (\_acct -> openAccount 1 0)
