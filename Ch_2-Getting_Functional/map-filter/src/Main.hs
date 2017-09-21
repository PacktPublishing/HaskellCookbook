module Main where

import Prelude hiding (map, filter)

map :: (a -> b) -> [a] -> [b]
map mapper [] = []
map mapper (x:xs) = mapper x : map mapper xs

map' :: (a -> b) -> [a] -> [b]
map' mapper xs = map1 xs []
  where
    map1 [] rs     = reverse rs
    map1 (x:xs) rs = map1 xs (mapper x : rs)

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)
  | f x       = x : filter f xs
  | otherwise = filter f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = filter1 xs []
  where
    filter1 [] rs = reverse rs
    filter1 (x:xs) rs 
      | f x       = filter1 xs (x : rs)
      | otherwise = filter1 xs rs


main :: IO ()
main = do
  let input = [1..10]
      squares = map (\x -> x * x) input
      squares' = map' (\x -> x * x) input
      odds = filter odd input
      odds' = filter' odd input
  putStrLn "Squaring [1..10]"
  putStrLn "Squares using map"
  putStrLn (show squares)
  putStrLn "Squares using tail recursive map"
  putStrLn (show squares')
  putStrLn "Filtering odd numbers in [1..10]"
  putStrLn "Using filter"
  putStrLn (show odds)
  putStrLn "Using tail recursive filter"
  putStrLn (show odds')
