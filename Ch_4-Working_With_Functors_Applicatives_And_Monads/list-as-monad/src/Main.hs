module Main where

import Control.Monad

nexts :: Num a => a -> [a]
nexts x = do
  x : nexts (x+1)

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
  x <- xs 
  y <- ys
  return (x,y)

partition :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
partition f xs ys = [ (x, y) | x <- xs, y <- ys, f x y]

partition1 :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
partition1 f xs ys = do
  x <- xs
  y <- ys
  if f x y then
    return (x,y)
    else
    []

main :: IO ()
main = do
  putStrLn "Next 10 elements from 11"
  print $ take 10 (nexts 11)
  putStrLn "Filtering out even elements from [1..1]"
  print $ filterM (\x -> if odd x then [True] else [False]) [1..10]
  putStrLn "Applying forM over a list and Maybe and embedding them in a list"
  print $ forM [1..10] (:[])
  print $ forM (Just 10) (:[])
  putStrLn "All pairs between [1..5] and ['a'..'c']"
  print $ pairs [1..5] ['a'..'c']
  putStrLn "Partition the ordered pairs between [i] and [j] such that i > j"
  print $ partition (>) [1..10] [1..10]
  putStrLn "Partition the ordered pairs between [i] and [j] such that i < j"
  print $ partition1 (<) [1..10] [1..10]
