module Main where

import Prelude hiding (Either(..))
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid

data Cursor = Cursor Int Int deriving Show

instance Monoid Cursor where

  mempty = Cursor 0 0

  (Cursor p q) `mappend` (Cursor r s) = Cursor (p + r) (q + s)

data Move = Up Int | Down Int | Left Int | Right Int deriving Show

toCursor :: Move -> Cursor
toCursor (Up p) = Cursor 0 (-p)
toCursor (Down q) = Cursor 0 q
toCursor (Left p) = Cursor (-p) 0
toCursor (Right q) = Cursor q 0

updateCursor :: Monad m => Move -> WriterT Cursor m ()
updateCursor = tell . toCursor

moveCursor :: [Move] -> ReaderT Cursor (WriterT Cursor IO) ()
moveCursor ms = do
  c <- ask
  lift $ tell c
  lift $ moveCursor' ms

  where
    moveCursor' [] = lift $ return ()
    moveCursor' (m:ms) = do
      lift $ putStrLn $ "Applying move " ++ (show m)
      updateCursor m
      moveCursor' ms

moves = [Up 10, Right 10, Down 20, Left 5]

main :: IO ()
main = do
  (_, cursor) <- runWriterT (runReaderT (moveCursor moves) (Cursor 10 10))
  putStrLn "Final cursor position"
  print cursor
