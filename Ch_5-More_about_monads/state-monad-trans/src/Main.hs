module Main where

import Data.Functor
import Control.Applicative
import Control.Monad

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where

  fmap f (StateT func) = let stateFunc s = (\(xa,xs) -> (f xa, xs)) <$> func s
                         in StateT stateFunc


instance Applicative m => Applicative (StateT s m) where

  -- Use the applicative instance of the embedded applicative to
  -- induce both x as well as state s into it.
  pure x = let stateFunc s = pure (x, s)
           in StateT stateFunc

  -- Get a function from State s m (a -> b) and apply it to
  -- State s m a to produce State s m b
  
  f <*> fa =
    let stateFunc s =
          let sf = runStateT f s   -- m (f :: a -> b, s)
              sa = runStateT fa s  -- m (a, s)
              -- Convert m (f :: a -> b, s) to
              -- m (f :: (a, s) -> (b, s) )
              func (fab, _) = (\(xa, st) -> (fab xa, st))
          in (func <$> sf) <*> sa
    in StateT stateFunc

instance Monad m => Monad (StateT s m) where

  return = pure

  sma >>= smab =
    let stateFunc s =
          let ma = runStateT sma s -- m (a, s)
          in do 
            (a, s1) <- ma
            runStateT (smab a) s1
    in StateT stateFunc

-- Implement get and put

get :: Monad m => StateT s m s
get = let stateFunc s = pure (s, s)
      in StateT stateFunc

put :: Monad m => s -> StateT s m ()
put s = let stateFunc _ = pure ((), s)
        in StateT stateFunc

lift :: Monad m => m a -> StateT s m a
lift ma = let stateFunc s = do
                a <- ma
                return (a, s)
          in StateT stateFunc
  

example :: Int -> StateT Int IO ()
example j = do
  i <- get
  lift $ putStrLn $ "Current state is " ++ (show i)
  put j
  i' <- get
  lift $ putStrLn $ "Current state is " ++ (show i')

    
main :: IO ()
main = do
  (_, state) <- runStateT (example 10) 100
  putStrLn $ "Result state is " ++ (show state)
  (_, state1) <- runStateT (example 1234) 12
  putStrLn $ "Result state is " ++ (show state1)
