module Main where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n | n < 0 = error "invalid index"
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
  putStrLn $ "f(0) = " ++ show (fib 0)
  putStrLn $ "f(1) = " ++ show (fib 1)
  putStrLn $ "f(5) = " ++ show (fib 5)
  putStrLn $ "f(10) = " ++ show (fib 10)
  putStrLn $ "f(20) = " ++ show (fib 20)
