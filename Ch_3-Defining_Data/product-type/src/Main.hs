module Main where


data Product1 = Product1 Bool deriving Show

data Product2 = Product2 Bool Bool deriving Show

data Product3 a = Product3 a Bool deriving Show

data Product4 a b = Product4 a b deriving Show


main :: IO ()
main = do
  putStrLn "Product1: Simple product type"
  putStrLn $ show $ Product1 True
  putStrLn $ show $ Product1 False

  putStrLn "Product2: Product type with two fields"
  putStrLn "Product2 has two boolean fields. Each one can take two values each"
  putStrLn $ show $ Product2 True True
  putStrLn $ show $ Product2 True False
  putStrLn $ show $ Product2 False True
  putStrLn $ show $ Product2 False False
  
  putStrLn "Product3: Product type with two fields, one parametric (Int)"
  putStrLn "Cardinality of Product3 is cardinality of Int multipled by two"
  putStrLn $ "which is " ++ (show (2 * (fromIntegral (maxBound :: Int) - fromIntegral (minBound :: Int) + 1)))
  let product3 = Product3 10 True :: Product3 Int
  putStrLn $ show product3

  putStrLn "Product4: Product type parametrized by two types (Int Bool)"
  putStrLn "Hence is equivalent to Product3 in these parameters"
  putStrLn $ show $ (Product4 10 True :: Product4 Int Bool)
 
