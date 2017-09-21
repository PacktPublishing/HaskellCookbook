module Main where

import Prelude hiding(Maybe(..))

import Data.Functor
import Control.Applicative
import Control.Monad

data Maybe a = Nothing | Just a deriving Show

instance Functor Maybe where

  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing

instance Applicative Maybe where

  pure x = Just x

  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)


instance Monad Maybe where

  return = Just 

  Nothing  >>= _ = Nothing
  (Just x) >>= f = f x 


add :: Num a => Maybe a -> Maybe a -> Maybe a
add x y = do
  i <- x
  j <- y
  return (i + j)

multiply :: Num a => Maybe a -> Maybe a -> Maybe a
multiply x y = liftM2 (*) x y

fromOdd :: Integral a => a -> Maybe a
fromOdd x | odd x = Just x
fromOdd _ = Nothing

isJust :: Maybe a -> Maybe Bool
isJust (Just _) = Just True
isJust Nothing  = Just False

main :: IO ()
main = do
  print $ multiply (Just 10) (Just 2)
  print $ multiply (Just 10) Nothing
  print $ add (Just 10) (Just 2)
  print $ add Nothing (Just 2)
  print $ forM [1..10] Just
  print $ forM [1..10] fromOdd
  print $ filterM (isJust . fromOdd) [1..10]
  print $ (pure 10 :: Maybe Int) >>= \x -> return (x * x)
