module Main where

data Days = Sunday
          | Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          deriving Show

data Variant a b c d e = Variant0 
                       | Variant1 a
                       | Variant2 b
                       | Variant3 c
                       | Variant4 d
                       | Variant5 e
                       deriving Show

main :: IO ()
main = do
  putStrLn $ "Sum Type 1 : Showing days of the week"
  putStrLn $ show [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
  putStrLn $ "Days type can have only 7 values"
  putStrLn ""
  putStrLn "Sum Type 2 : Variant with 5 possible data constructors"
  putStrLn "Each constructor contribues number of possible values"
  putStrLn "of types a, b, c, d, e or f"

  let v0 = Variant0 :: Variant Int Float Double Char String
      v1 = Variant1 10 :: Variant Int Float Double Char String
      v2 = Variant2 11.0 :: Variant Int Float Double Char String
      v3 = Variant3 12.0 :: Variant Int Float Double Char String
      v4 = Variant4 'A' :: Variant Int Float Double Char String
      v5 = Variant5 "Haskell" :: Variant Int Float Double Char String

  putStrLn "Showing all variants"
  putStrLn $ show [v0,v1,v2,v3,v4,v5]
  putStrLn "Variant0 has only one value, however its type is completely qualified"
