module Main where

import Data.Functor

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

node :: Tree a -> a -> Tree a -> Tree a
node l x r = Node l x r

singleton :: a -> Tree a
singleton x = Node Leaf x Leaf

instance Functor Tree where

  fmap _ Leaf = Leaf
  fmap f (Node left value right) = Node (fmap f left) (f value) (fmap f right)

sampleTree :: Tree Int
sampleTree = node l 1 r
  where
    l = node ll 2 rl
    r = node lr 3 rr
    ll = node lll 4 rll
    rl = node lrl 5 rrl
    lr = node llr 6 rlr
    rr = node lrr 7 rrr
    lll = singleton 8
    rll = singleton 9
    lrl = singleton 10
    rrl = singleton 11
    llr = singleton 12
    rlr = singleton 13
    lrr = singleton 14
    rrr = singleton 15

main :: IO ()
main = do
  let intTree = sampleTree
      stringTree = fmap show intTree
      intTree1 = fmap (read :: String -> Int) stringTree
  putStrLn "Original Tree"
  print intTree
  putStrLn "Tree of integers to Tree of strings"
  print stringTree
  putStrLn "Tree of strings converted back to Tree of integers is same as original tree?"
  print $ intTree == intTree1
