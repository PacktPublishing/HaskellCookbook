module Main where

import qualified Control.Exception as E
import System.IO

div1 :: Int -> Int -> Int
div1 x 0 = error "Division by zero"
div1 x y = x `div` y

safeDiv1 :: Int -> Int -> IO ()
safeDiv1 x y = E.catch (putStrLn $ show $ div1 x y) (\e -> putStrLn (show $ (e :: E.SomeException)))

safeDiv2 :: Int -> Int -> IO ()
safeDiv2 x y = do
  result <- E.try (putStrLn $ show $ div1 x y)
  case result of
    Left e -> putStrLn $ show (e :: E.SomeException)
    Right r -> putStrLn $ show r

safeReadFile :: FilePath -> IO String
safeReadFile filepath = do
  E.catch (readFile filepath)
    (\e -> do
        putStrLn $ "ERROR " ++ (show (e :: E.IOException))
        return "" )


data Point = Point Float Float deriving Show
data Line = Line Point Point deriving Show

distanceSq :: Point -> Point -> Float
distanceSq (Point x1 y1) (Point x2 y2) = xx + yy
  where
    square t = t * t
    xx = square (x1 - x2)
    yy = square (y1 - y2)

tolerance :: Float
tolerance = 1e-6


data GeometryException = ZeroLengthLine

instance Show GeometryException where

  show ZeroLengthLine = "Line with zero or less than tolerance length"

instance E.Exception GeometryException

safeLine :: Point -> Point -> Line
safeLine p1 p2 | distanceSq p1 p2 < tolerance = E.throw ZeroLengthLine
safeLine p1 p2 = Line p1 p2

showLine :: Line -> IO ()
showLine line =
  E.catch (putStrLn $ show line)
  (\e -> do
      putStrLn $ "ERROR " ++ (show (e :: GeometryException))
      return ()
  )


main :: IO ()
main = do
  -- Catch all exceptions
  safeDiv1 4 2
  safeDiv1 7 0
  -- Using try just
  safeDiv1 12 2
  safeDiv1 7 0

  -- Safe file read
  contents <- safeReadFile "some-arbitrary-name"
  putStrLn "The contents should be blank"
  putStr contents

  let p1 = Point 10 10
      p2 = Point 0 0

  putStrLn "Line with zero length"
  showLine (safeLine p1 p1)
  putStrLn "Valid line"
  showLine (safeLine p1 p2)
