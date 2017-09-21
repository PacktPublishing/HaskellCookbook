module Main where

import Control.Monad.Writer.Strict
import Data.Monoid

newtype Transaction = Transaction Double deriving Show

printTransaction :: Transaction -> IO ()
printTransaction (Transaction x) | x < 0 = putStrLn $ "Debiting " ++ (show x)
printTransaction (Transaction x) | x > 0 = putStrLn $ "Crediting " ++ (show x)
printTransaction (Transaction x) = putStrLn "No Change"

instance Monoid Transaction where

  mempty = Transaction 0

  (Transaction x) `mappend` (Transaction y) = Transaction (x + y)


balanceSheet :: [Transaction] -> WriterT Transaction IO ()
balanceSheet [] = lift $ putStrLn "Finished balancing"
balanceSheet (b:bs) = do
  tell b
  lift $ printTransaction b
  balanceSheet bs

transactions = [ Transaction (-10.0)
               , Transaction 5
               , Transaction 17
               , Transaction (-29)
               , Transaction 10]

main :: IO ()
main = do
  (_, Transaction b) <- runWriterT (balanceSheet transactions)
  putStrLn $ "Balance is " ++ (show b)
