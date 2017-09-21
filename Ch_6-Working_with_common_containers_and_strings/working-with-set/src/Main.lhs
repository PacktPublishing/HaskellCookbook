Working with Sets
=================

In this recipe, we will work with Set and its APIs. A set is a
container which stores unique ordered values. 

How to do it...
---------------

  <ul>
  <li> Create a new project working-with-set using simple stack template

~~~  
  stack new working-with-set simple
~~~

  <li> In the project folder, open working-with-set.cabal file. Add the new dependency *containers* in the build-depends subsection of section *executable*. 

~~~  
  executable working-with-set
    hs-source-dirs:      src
    main-is:             Main.lhs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , Containers
~~~

  The containers library is a commonly used library which implements
  containers such as set, map etc.

> module Main where

  <li>Import Data.Set for using set and related functions.

> import Data.Set as S

  <li>Write main, we will be writing set examples in the main function

> main :: IO ()
> main = do

  <li>Create an empty set

>   let emptyS = S.empty :: S.Set String
>   putStrLn "Empty String Set"
>   print emptyS

  <li>Create a singleton set

>   let singleS = S.singleton "Single"
>   putStrLn "Singleton Set"
>   print singleS

  <li>Insert a string into a set

>   let insS = S.insert "Another" singleS
>   putStrLn "Singleton with insertion"
>   print insS

  <li>Get size of the set

>   putStrLn "Size of the set"
>   print $ S.size insS

  <li>Create a set from a list

>   let fromL = S.fromList [0..9] :: S.Set Int
>   putStrLn "Set from list"
>   print fromL

  <li>Create the list from the set

>   let toL = S.toList fromL
>   putStrLn "List from set"
>   print toL

  <li>Create the ascending and descending lists from set

>   let toAscL = S.toAscList fromL
>   let toDscL = S.toDescList fromL
>   putStrLn "Set to ascending and descending lists"
>   print toAscL
>   print toDscL

  <li>Remove minimum and maximum element

>   let removeMinS = S.deleteMin fromL
>   let removeMaxS = S.deleteMax fromL
>   putStrLn "Removing minimum and maximum elements"
>   print removeMinS
>   print removeMaxS

  <li>Union and intersection of two sets

>   putStrLn "Take two sets [0..9] and [5..15]"
>   let fS = S.fromList [0..9]
>   let sS = S.fromList [5..15]
>   let intS = S.intersection fS sS
>   let uniS = S.union fS sS
>   putStrLn "Printing intersection and union respectively"
>   print intS
>   print uniS

  <li>Lookup an element greater than and greater than and equal to an element.

>   putStrLn "Construct set from list [1, 2, 4]"
>   let exS = S.fromList [1,2,4] :: S.Set Int
>   putStrLn "Find element greater than 2"
>   print $ S.lookupGT 2 exS
>   putStrLn "Find element greater than or equal to 2"
>   print $ S.lookupGE 2 exS

  <li>Is an item element of set?

>   putStrLn "Find if 4 is part of the set from [0..9]"
>   print $ S.member 4 fromL

  <li>Fold over the list - In this case we will use both foldr and foldr to fold over elements in the set. In this case we use folds to create a list of items from the elements in the fold.

>   putStrLn "Fold over the list, foldr and foldl"
>   print $ S.foldr (:) [] fromL
>   print $ S.foldl (flip (:)) [] fromL


How did we do it...
-------------------

A set is a ordered collection of unique items. If we insert an item, which is already present in the set, then we get the same set back. One important difference between  collections in Haskell and other languages is that the collections in Haskell (as other data types) are immutable. The set is implemented as a binary tree. 

