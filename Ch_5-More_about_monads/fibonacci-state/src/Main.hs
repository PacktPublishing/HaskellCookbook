module Main where

import Control.Applicative
import Control.Monad.State
import Data.Map.Strict as M

type FibMap a = Map a a

type FibState a b = State (FibMap a) b

-- Get the state, lookup
getFib :: Integral a => a -> FibState a (Maybe a)
getFib i = M.lookup i <$> get 

-- Get the map, insert the number, save the state
putFib :: Integral a => a -> a -> FibState a a
putFib i v = do
  mp <- (pure $ M.insert i v) <*> get
  put mp
  return v

fibWithState :: Integral a => a -> FibState a a
fibWithState i | i == 0 || i == 1 = do
                   f <- getFib i
                   case f of
                     Just v  -> return v
                     Nothing -> putFib i i
fibWithState n = do
  n_1 <- getFibOr (n-1)
  n_2 <- getFibOr (n-2)
  putFib n (n_1 + n_2)

  where
    getFibOr m = do
      fm <- getFib m
      case fm of
        Just fv -> return fv
        Nothing -> fibWithState m

main :: IO ()
main = do
  let mp = execState (fibWithState 30) M.empty
  putStrLn "Calling fibWithState 30, would sore fibonacci number till 30 in the map"
  print mp
  putStrLn "Calling any fibonacci number till 30 is memoized, and will be only looked up"
  print $ evalState (fibWithState 15) mp
