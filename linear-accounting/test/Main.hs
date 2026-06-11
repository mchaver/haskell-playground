{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for the conservation law the linear API is meant to encode.
--
-- Linearity makes \"funds can't be duplicated or dropped\" a /compile-time/
-- fact, so these runtime properties don't re-prove that — they pin down the
-- arithmetic that linearity lets us trust: 'split', 'merge', and 'transfer'
-- conserve the grand total, and a transfer succeeds exactly when the source
-- can cover it.
module Main (main) where

import Accounting.Ledger (closeAccount, openAccount, transfer)
import Accounting.Money (Money, burn, merge, mint, split)
import Data.Unrestricted.Linear (Ur (Ur))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (exitFailure, exitSuccess)
import Prelude

-- | Magnitudes kept well under half of 'Word64' so sums never overflow.
genAmount :: Gen Word
genAmount = Gen.word (Range.linear 0 1_000_000_000)

-- | Drain a 'Money' to its plain magnitude (consumes it, as linearity requires).
valueOf :: Money %1 -> Word
valueOf m = case burn m of
  Ur a -> fromIntegral a

-- | @split@ partitions the total: the two pieces sum back to the input.
prop_split_conserves :: Property
prop_split_conserves = property $ do
  total <- forAll genAmount
  n <- forAll genAmount
  let pieces = case split (fromIntegral n) (mint (fromIntegral total)) of
        Left whole -> valueOf whole
        Right (a, b) -> valueOf a + valueOf b
  pieces === total

-- | @split@ succeeds (returns 'Right') exactly when the pile covers @n@.
prop_split_succeeds_iff_funded :: Property
prop_split_succeeds_iff_funded = property $ do
  total <- forAll genAmount
  n <- forAll genAmount
  let funded = case split (fromIntegral n) (mint (fromIntegral total)) of
        Left _ -> False
        Right _ -> True
  funded === (n <= total)

-- | @merge@ adds two piles without losing or inventing value.
prop_merge_conserves :: Property
prop_merge_conserves = property $ do
  x <- forAll genAmount
  y <- forAll genAmount
  valueOf (merge (mint (fromIntegral x)) (mint (fromIntegral y))) === (x + y)

-- | @transfer@ conserves the grand total across both accounts, whether or not
-- it actually moved anything.
prop_transfer_conserves :: Property
prop_transfer_conserves = property $ do
  x <- forAll genAmount
  y <- forAll genAmount
  n <- forAll genAmount
  let (a, b, _ok) = transfer (fromIntegral n) (openAccount 1 (fromIntegral x)) (openAccount 2 (fromIntegral y))
      Ur la = closeAccount a
      Ur lb = closeAccount b
  (fromIntegral la + fromIntegral lb) === (x + y)

-- | When @transfer@ reports success, the source really was debited by @n@ and
-- the destination credited by the same — and when it reports failure, both
-- balances are exactly as opened.
prop_transfer_moves_n_on_success :: Property
prop_transfer_moves_n_on_success = property $ do
  x <- forAll genAmount
  y <- forAll genAmount
  n <- forAll genAmount
  let (a, b, Ur ok) = transfer (fromIntegral n) (openAccount 1 (fromIntegral x)) (openAccount 2 (fromIntegral y))
      Ur la = closeAccount a
      Ur lb = closeAccount b
  if ok
    then do
      fromIntegral la === (x - n)
      fromIntegral lb === (y + n)
    else do
      fromIntegral la === x
      fromIntegral lb === y

main :: IO ()
main = do
  ok <-
    checkParallel $
      Group
        "linear-accounting conservation"
        [ ("split conserves the total", prop_split_conserves)
        , ("split succeeds iff funded", prop_split_succeeds_iff_funded)
        , ("merge conserves the total", prop_merge_conserves)
        , ("transfer conserves the grand total", prop_transfer_conserves)
        , ("transfer moves exactly n on success", prop_transfer_moves_n_on_success)
        ]
  if ok then exitSuccess else exitFailure
