module Main where

import Quadratic
import Data.Complex

main :: IO ()
main = do
  putStrLn $ show $ roots (Quadratic 0 1 2)
  putStrLn $ show $ roots (Quadratic 1 3 4)
  putStrLn $ show $ roots (Quadratic 1 3 4)
  putStrLn $ show $ roots (Quadratic 1 4 4)
  putStrLn $ show $ roots (Quadratic 1 0 4)
