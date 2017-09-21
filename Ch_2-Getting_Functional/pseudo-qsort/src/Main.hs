module Main where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
  where
    ys = filter (\y -> y < x) xs
    zs = filter (\z -> z >= x) xs

main :: IO ()
main = do
  let input = [5,2,3,1,7,9,8,4,6,0]
      sorted = qsort input
  putStrLn $ "input: " ++ (show input)
  putStrLn $ "sorted: " ++ (show sorted)

