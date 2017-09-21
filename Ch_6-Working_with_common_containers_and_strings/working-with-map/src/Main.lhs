Working with Map
================

In this recipe, we will be looking at Data.Map. A map keeps an association between key and the corresponding value. The map stores the ordered key and their values (dictionaries). There are two variants in the container library, strict and lazy. We will be looking at strict variant. The lazy variant has same interface, except that the implementation is lazy.


How do we do it...
------------------
  <ul>

  <li>Create a new project work-with-map using *simple* stack template.

  <li>Add containers library to the build-depends subsection of the executable subsection.

~~~
    executable working-with-map
      hs-source-dirs:      src
      main-is:             Main.hs
      default-language:    Haskell2010
      build-depends:       base >= 4.7 && < 5
                         , containers
~~~

  <li>Open _src/Main.hs_, we will be using this as our playground for dealing with map.

> module Main where

  <li>Import _Data.Map_ to use the map functions. We will be using strict version of map.

> import Data.Map.Strict as M

  <li>We will be using main function directly to work with map functions

> main :: IO ()
> main = do
>   

  <li>**Map Construction** - Create an empty map or a map with single entry

>   let e = M.empty :: Map Int Int
>   let s = M.singleton 1 "Haskell Curry" :: Map Int String
>   putStrLn "Empty and singleton maps"
>   print e
>   print s

  <li>It is also possible to create map from the list of key and values

>   let ml = M.fromList [(1, "Alphonso Church"), (2, "Haskell Curry")] :: Map Int String
>   putStrLn "Map from list"
>   print ml

  <li>**Insert into Map**

  While inserting we have to deal with the fact that the key might already there in the map. Map implementation deals with this fact by providing variants to manage different needs.

>   let ml1 = M.insert 3 "Alan Turing" ml
>   putStrLn "Inserting into map"
>   print ml1
>   -- [(1,"Alphonso Church"),(2, "Haskell Curry"), (3, "Alan Turing")]

  <li>**Replace a value** in a map

>   print $ M.insert 2 "Haskell Curry, Haskell inspiration" ml1
>   -- [(1,"Alphonso Church"),(2, "Haskell Curry, Haskell inspiration"), (3, "Alan Turing")]

  <li>Inserting by looking at old value and combining it with new value.

>   -- Word count in a para
>   let iml = M.fromList [("a",10), ("an",2), ("the", 8)] :: Map String Int
>   print (M.insertWith (+) "a" 2 iml)
>   -- [("a",12),("an",2),("the",8)]
  
  <li>**Delete from the map**

  Remove the article "the" from the above map. We provide a key to delete key value pair from the container. 

>   print (M.delete "the" iml)
>   -- [("a",10),("an",2)]

  <li>**Update the map**

  Use a variant updateWithKey. It takes a function which looks at a key and the value, and possibly produces another value. This can result in either replacing a value or deleting the key value pair in the map. Define the *updater* function. It changes value of **an** in the article map we have created above. It also deletes the key **the** from the map. Everything else is kept same.

>   let updater "an" _ = Just 5
>       updater "the" 8 = Nothing
>       updater _  v   = Just v
>   print $ M.updateWithKey updater "an" iml
>   -- [("a",10),("an",5),("the",8)]
>   print $ M.updateWithKey updater "the" iml
>   -- [("a",10),("an",2)]

  <li>**Union and Difference of maps**

  Two maps can be combined to create a single map. We use _unionWith_ variant here. We create another word count map and combine it here. We add the counts if duplicate entries are found. This is done by supplying (+) function to _unionWith_.

>   let iml1 = M.fromList [("a",3),("and",6)]
>   print $ M.unionWith (+) iml iml1
>   -- [("a",13),("an",2),("and",6),("the",8)]

  <li>Similarly, we can find the difference between two maps. We use _differenceWith_ where we can select whether to keep the value in the first map or not. Use _differenceWith_ if you'd like to remove the duplicate keys. Note that the second map that the difference function takes, does not have same value type. The key type should be same though.

>   let iml2 = M.fromList [("an",False),("the",True)]
>   let diff v False = Nothing
>       diff v _     = Just v
>   print $ M.differenceWith diff iml iml2
>   -- [("a",10),("the",8)]

  <li>Find the intersection of two maps. The intersection function prefers the value int the first map. The _intersectionWith_ function lets you decide what to do with the values. Here we use intersection function.

>   print $ M.intersection iml iml1
>   -- [("a",10)]

  <li>Build and execute the project.

~~~
  stack build
  stack exec -- working-with-map
~~~

  The program output should agree with out expected answers,
  
  </ul>

How did we do it...
-------------------

 We have seen common functions for dealing with map. Typically the function without suffix _With_, or _WithKey_ have default behavior. For example, in insert, the default behavior is to replace existing value. The suffix _With_ takes two values to produce a new value. The suffix _WithKey_ also takes into consideration the key for which we are combining values.

