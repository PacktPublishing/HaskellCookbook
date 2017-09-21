module Main where

import Control.Monad.Reader

example :: ReaderT Int IO ()
example = do
  s <- ask
  lift $ putStrLn $ "Current env state is " ++ (show s)

  s_is_10 <- asks (== 10)
  lift $ putStrLn $ "Current state is 10? " ++ (show s_is_10)


cover :: ReaderT Int IO ()
cover = do
  example
  local (const 10) example


main :: IO ()
main = runReaderT cover 100
