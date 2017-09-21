module Main where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' :: [a] -> [a] -> [a]
    reverse' [] rs = rs
    reverse' (x:xs) rs = reverse' xs (x:rs)

main :: IO ()
main = do
  let inp = [1..10]
      rs = reverse [1..10]
  putStrLn $ "Reverse of " ++ (show inp) ++ " is " ++ (show $ reverse inp)
