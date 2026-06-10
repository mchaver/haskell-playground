-- | A tiny scripted scenario that prints trades and the resulting book, so you
-- can see the engine work without reading the test suite.
module Main (main) where

import           Control.Monad   (foldM)
import           Data.Maybe      (catMaybes)

import           OrderBook.Book  (Book, asks, bestAsk, bestBid, bids, emptyBook,
                                  match, mrBook, mrStatus, mrTrades)
import           OrderBook.Types (NewOrder (..), OrderId (..), OrderType (..),
                                  Price (..), Side (..), TimeInForce (..),
                                  mkQuantity)

-- | Build a limit order, dropping it if the size is not strictly positive.
-- Returns 'Maybe' rather than calling 'error' so the helper stays total.
limit :: OrderId -> Side -> Int -> Int -> TimeInForce -> Maybe NewOrder
limit oid side px sz tif =
  NewOrder oid side (Limit (Price px)) tif <$> mkQuantity sz

-- | Apply an order, print what happened, and return the new book.
step :: Book -> NewOrder -> IO Book
step book order = do
  let res = match order book
  putStrLn $ "submit " ++ show order
  putStrLn $ "  status: " ++ show (mrStatus res)
  mapM_ (\t -> putStrLn $ "  trade:  " ++ show t) (mrTrades res)
  pure (mrBook res)

main :: IO ()
main = do
  -- Build a resting book: bids 99/100, asks 101/102.
  let flow =
        catMaybes
        [ limit (OrderId 1) Sell 102 5 GTC
        , limit (OrderId 2) Sell 101 5 GTC
        , limit (OrderId 3) Buy  100 5 GTC
        , limit (OrderId 4) Buy   99 5 GTC
        -- A buy that sweeps both ask levels and rests the remainder at 103.
        , limit (OrderId 5) Buy  103 12 GTC
        ]
  book <- foldM step emptyBook flow

  putStrLn "\nfinal book:"
  putStrLn $ "  bids: " ++ show (bids book)
  putStrLn $ "  asks: " ++ show (asks book)
  putStrLn $ "  best bid / ask: " ++ show (bestBid book) ++ " / " ++ show (bestAsk book)
