module Main where

import Lib

main :: IO ()
main = do
  putStrLn $ show $ (emptyList :: [Int])
  putStrLn $ show $ prepend
  putStrLn $ show $ list5
  putStrLn $ show $ list10
  putStrLn $ show $ getHead
  putStrLn $ show $ getTail
  putStrLn $ show $ allbutlast
  putStrLn $ show $ take10
  putStrLn $ show $ drop10
  putStrLn $ show $ get1331th
  putStrLn $ show $ is10element
  putStrLn $ show $ isEmpty [10]
  putStrLn $ show $ isSize2 []
  putStrLn $ show $ cat2
  putStrLn $ show $ a2z
  putStrLn $ show $ strHead
