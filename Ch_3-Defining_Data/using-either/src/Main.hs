module Main where

import Data.Either

safeDiv :: Either String Int -> Either String Int -> Either String Int
safeDiv (Left e) _ = Left e
safeDiv _ (Left e) = Left e
safeDiv (Right i) (Right j) | j == 0 = Left "Illegal Operation: Division by Zero"
safeDiv (Right i) (Right j) = Right (i `div` j)

main :: IO ()
main = do
  let i = Right 10 :: Either String Int
      j = Right 2 :: Either String Int
      z = Right 0 :: Either String Int

      
  putStrLn $ "Safe division : 10 / 2 = " ++ (show $ safeDiv i j)
  putStrLn $ "Safe division : 10 / 0 = " ++ (show $ safeDiv i z)
