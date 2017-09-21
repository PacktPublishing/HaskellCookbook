module Main where

fib n = fiblist !! n

fiblist = 0 : 1 : zipWith (+) fiblist (tail fiblist)

main :: IO ()
main = do
  let fib10k = fib 10000
  putStrLn $ "10000th fibonacci number is " ++ (show fib10k)
