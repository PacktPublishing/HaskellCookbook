module Main where

sumr :: Num a => [a] -> a
sumr xs = foldr (+) 0 xs

suml :: Num a => [a] -> a
suml xs = foldl (+) 0 xs

productr :: Num a => [a] -> a
productr xs = foldr (*) 1 xs

productl :: Num a => [a] -> a
productl xs = foldl (*) 1 xs

mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x result -> f x : result) [] xs

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\result x -> f x : result) [] xs

filterr :: (a -> Bool) -> [a] -> [a]
filterr f xs = foldr filtered [] xs
  where
    filtered x result
      | f x       = x : result
      | otherwise = result

filterl :: (a -> Bool) -> [a] -> [a]
filterl f xs = foldl filtered [] xs
  where
    filtered result x
      | f x = x : result
      | otherwise = result

main :: IO ()
main = do
  let input = [1..10]
      square x = x * x
  putStrLn "Calculating sum of [1..10]"
  putStrLn "  Using foldr"
  putStrLn (show $ sumr input)
  putStrLn "  Using foldl"
  putStrLn (show $ suml input)

  putStrLn "Calculating product of [1..10]"
  putStrLn "  Using foldr"
  putStrLn (show $ productr input)
  putStrLn "  Using foldl"
  putStrLn (show $ productl input)

  putStrLn "Squaring [1..10] using map"
  putStrLn "  Using foldr"
  putStrLn (show $ mapr square input)
  putStrLn "  Using foldl"
  putStrLn (show $ mapl square input)

  putStrLn "Filtering odd elements [1..10] using filter"
  putStrLn "  Using foldr"
  putStrLn (show $ filterr odd input)
  putStrLn "  Using foldl"
  putStrLn (show $ filterl odd input)
