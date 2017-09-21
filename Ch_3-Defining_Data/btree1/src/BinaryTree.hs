module BinaryTree where

data BinaryTree a = Leaf
                  | BinaryTree { left :: BinaryTree a,
                                 val :: a,
                                 right :: BinaryTree a }
                    deriving Show


empty :: BinaryTree a
empty = Leaf

dfTraverse :: BinaryTree a -> [a]
dfTraverse Leaf = []
dfTraverse tree = dfTraverse (left tree)  ++ [val tree] ++ dfTraverse (right tree)

bfTraverse :: BinaryTree a -> [a]
bfTraverse Leaf = []
bfTraverse tree = bfTraverse1 [tree] []
  where
    bfTraverse1 [] xs = [] (reverse xs)
    bfTraverse1 (Leaf:trees) xs = bfTraverse1 trees xs
    bfTraverse1 (tree:trees) xs = bfTraverse1 (left tree:right tree:trees) (val tree:xs)
