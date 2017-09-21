module Main where

data BinaryTree a = Leaf
                  | BinaryTree { left :: BinaryTree a,
                                 val :: a,
                                 right :: BinaryTree a }
                    deriving Show


empty :: BinaryTree a
empty = Leaf

singleton :: a -> BinaryTree a
singleton x = BinaryTree Leaf x Leaf

node :: BinaryTree a -> a -> BinaryTree a -> BinaryTree a
node l x r = BinaryTree { left = l, val = x, right = r }

dfTraverse :: BinaryTree a -> [a]
dfTraverse Leaf = []
dfTraverse tree = dfTraverse (left tree)  ++ [val tree] ++ dfTraverse (right tree)

bfTraverse :: BinaryTree a -> [a]
bfTraverse Leaf = []
bfTraverse tree = bfTraverse1 [tree] [] []
  where
    bfTraverse1 [] [] xs = reverse xs
    bfTraverse1 [] q  xs = bfTraverse1 (reverse q) [] xs
    bfTraverse1 (Leaf:ts) q xs = bfTraverse1 ts q xs
    bfTraverse1 (t:ts) q xs = bfTraverse1 ts (right t:left t:q) (val t:xs)

sampleTree :: BinaryTree Int
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
  let tree = sampleTree
      inorder = dfTraverse tree
      bfs = bfTraverse tree
  putStrLn "In order depth first traversal"
  print inorder
  
  putStrLn "Breadth first traversal"
  print bfs
