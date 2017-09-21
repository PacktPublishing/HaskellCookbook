module Fibonacci where

fib :: Int -> Int
fib n | n < 0 = error "Negative index"
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fib1 n | n < 0 = error "Negative index"
fib1 n = fibworker n 1 0
  where
    fibworker :: Int -> Int -> Int -> Int
    fibworker 0 _ _ = 0
    fibworker 1 _ _ = 1
    fibworker 2 p1 p2 = p1 + p2
    fibworker n p1 p2 = fibworker (n-1) (p1+p2) p1
