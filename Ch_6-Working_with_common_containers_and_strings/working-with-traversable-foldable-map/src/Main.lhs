
Working with Traversable and Foldable 
=====================================

In this recipe, we will be working with two type classes, viz., *Traversable*, and *Foldable*. Both these classes are the generalization of the functions that we have seen working with *Lists*. The *Traversable*, as the name suggests allows us to browse a data structure to traverse from left to right. Similarly, the *Foldable* type class allows us to *fold* the elements of a data type.

In fact, in the previous versions of GHC, traversals and folding were defined for list. In the recent versions, those functions are generalized to include traversable and foldable, making them applicable to wide range of data structures.

We will also define *Traversable* and *Foldable* instance for a tree. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-traversable-foldable-map* with *simple* stack template.

~~~
  stack new working-with-traversable-foldable-map simple
~~~

  <li> Add dependency on containers library in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-traversable-foldable-map
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , containers
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Import the *Map* module.

> import Data.Map.Strict as M hiding (foldr)

  <li> Create a list of tuple of *Int* and *String*

> a2z :: [(Int,String)]
> a2z = zipWith (\i c -> (i,c:[])) [1..] ['a'..'z']

  <li> Create map from the above list.

> imap :: M.Map Int String
> imap = M.fromList a2z

  <li> The *Map* is an instance of *Foldable*. Use *foldMap* and *foldr* to concatenate all the values, and combine all the values. Use *id* function in *foldMap* to just concatenate the strings.

> ifold :: M.Map Int String -> String
> ifold = foldMap id
>
> ifoldr :: M.Map Int String -> String
> ifoldr = foldr (\s t -> s ++ ", " ++ t) ""

  <li> Use *Traversable* instance to traverse over the map.

> itraverse :: M.Map Int String -> [Map Int String]
> itraverse = traverse (\x -> [x ++ "-travsered" ])

  <li> Implement *Foldable* and *Traversable* instance for a tree. Define the binary tree!

> data Tree a = Empty
>             | Tree (Tree a) a (Tree a)
>               deriving Show

  <li> Implement *Foldable* instance.

> instance Foldable Tree where
>
>   foldMap f Empty = mempty
>   foldMap f (Tree left v right) = (foldMap f left) `mappend` f v `mappend` (foldMap f right)
>
>   foldr f x Empty = x
>   foldr f x (Tree left v right) = foldr_left
>     where
>       foldr_right = foldr f x right
>       foldr_value = f v foldr_right
>       foldr_left = foldr f foldr_value left

  <li> Implement *Traversable* instance for the *Tree*. For a *Travesable*, a *Functor* instance is also required.

> instance Functor Tree where
>   fmap f Empty = Empty
>   fmap f (Tree left v right) = Tree (fmap f left) (f v) (fmap f right)

> instance Traversable Tree where
>
>   traverse f Empty = pure Empty
>   traverse f (Tree left v right) = Tree <$> traverse f left <*> f v <*> traverse f right
>   

  <li> Create a sample tree.

> sampleTree :: Tree Int
> sampleTree = Tree l 10 r
>   where
>     l = Tree ll 8 lr
>     ll = Tree Empty 7 Empty
>     lr = Tree Empty 9 Empty
>     r = Tree rl 12 rr
>     rl = Tree Empty 11 Empty
>     rr = Tree Empty 13 Empty

  <li> Fold the tree to find sum of values of all the nodes.

> sampleSum :: Tree Int -> Int
> sampleSum = foldr (+) 0 

  <li> Traverse the tree, to create list of all the values in the node.

> sampleTraverse :: Tree Int -> [Tree String]
> sampleTraverse = traverse (\x -> [show x])

  <li> Use the map examples, and tree to fold and traverse.

> main :: IO ()
> main = do
>   putStrLn "Given the map"
>   print imap
>
>   putStrLn "Fold the map"
>   print $ ifold imap
>   print $ ifoldr imap
>
>   putStrLn "Traverse the map"
>   print $ itraverse imap
>
>   putStrLn "Given a tree"
>   print sampleTree
>
>   putStrLn "Folding the tree (Find the sum)"
>   print $ sampleSum sampleTree
>
>   putStrLn "Traverse the tree (create a list)"
>   print $ sampleTraverse sampleTree

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-traversable-foldable-map
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

*Foldable* and *Traversable* are very generic and can be defined for variety of data types. Instances of these type classes allow us defined a traversal and foldable, and use common functions such as *foldr*, *foldl* etc to variety of data structures. 

