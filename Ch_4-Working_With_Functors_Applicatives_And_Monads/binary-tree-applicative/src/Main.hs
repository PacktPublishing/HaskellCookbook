module Main where

import Data.Functor
import Control.Applicative

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)


instance Functor Tree where

  fmap _ Leaf = Leaf
  fmap f (Node left value right) = Node (fmap f left) (f value) (fmap f right)


instance Applicative Tree where

  pure x = let t = Node t x t
           in t

  (<*>) Leaf _ = Leaf
  (<*>) _ Leaf = Leaf
  (<*>) (Node lf f rf) (Node la a ra) = Node (lf <*> la) (f a) (rf <*> ra)

singleton :: a -> Tree a
singleton x = Node Leaf x Leaf

node :: Tree a -> a -> Tree a -> Tree a
node l x r = Node l x r

sampleTree :: Int -> Tree Int
sampleTree b = node l b r
  where
    l = node ll (b+1) rl
    r = node lr (b+2) rr
    ll = node lll (b+3) rll
    rl = node lrl (b+4) rrl
    lr = node llr (b+5) rlr
    rr = node lrr (b+6) rrr
    lll = singleton (b+7)
    rll = singleton (b+8)
    lrl = singleton (b+9)
    rrl = singleton (b+10)
    llr = singleton (b+11)
    rlr = singleton (b+12)
    lrr = singleton (b+13)
    rrr = singleton (b+14)


main :: IO ()
main = do
  let intTree1 = sampleTree 1
      intTree2 = sampleTree 15
      finalTree = (+) <$> intTree1 <*> intTree2
  putStrLn "First Tree"
  print intTree1
  putStrLn "Second Tree"
  print intTree2
  putStrLn "Final Tree"
  print finalTree
  putStrLn "Checking Applicatives Laws"
  putStrLn "Identity Law: pure id <*> v == v"
  putStrLn "pure id <*> intTree1 == intTree1"
  print $ (pure id <*> intTree1) == intTree1
  putStrLn "Homomorphism: pure f <*> pure x == pure (f x)"
  putStrLn "This property is not possible to test here, as pure produces infinite tree"
  putStrLn "Interchange: u <*> pure y == pure ($ y) <*> u"
  putStrLn "This property is not possible to test here, as pure produces infinite tree"
  putStrLn "Composition: pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
  let square x = x * x
      double x = x + x
  putStrLn "(pure (.) <*> pure square <*> pure double <*> intTree1) == (pure square <*> (pure double <*> intTree1))"
  print $ (pure (.) <*> pure square <*> pure double <*> intTree1) == (pure square <*> (pure double <*> intTree1))
  
