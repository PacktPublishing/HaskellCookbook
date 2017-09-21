module Main where

-- Group elements in groups of twos, but when we group it we keep them
-- sorted. 
group2 :: Ord a => [a] -> [[a]]
group2 [] = []
-- A single element is already sorted.
group2 (x:[]) = [[x]]
-- Create groups of two and sort them
group2 (x:y:xs) = (sort x y) : group2 xs
  where
    sort x y | x >= y    = y : x : []
             | otherwise = x : y : []

-- Assume that two lists are sorted, and merge them in the increasing
-- order. 
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x >= y    = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = mergeSort' (group2 xs)
  where
    mergeSort' :: Ord a => [[a]] -> [a]
    mergeSort' [] = []
    mergeSort' (xs:[]) = xs
    mergeSort' xss = mergeSort' (mergeStep' xss)
    
    mergeStep' :: Ord a => [[a]] -> [[a]]
    mergeStep' [] = []
    mergeStep' (xs:[]) = [xs]
    mergeStep' (xs:ys:xss) = (merge xs ys) : mergeStep' xss

main :: IO ()
main = do
  let input = [5,2,3,1,7,9,8,4,6,0]
      sorted = mergeSort input
  putStrLn $ "input: " ++ (show input)
  putStrLn $ "sorted: " ++ (show sorted)
