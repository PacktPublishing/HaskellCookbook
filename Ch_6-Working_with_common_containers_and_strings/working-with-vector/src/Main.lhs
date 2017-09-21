Working with Vectors
====================
In this recipe, we will be looking at Data.Vector from vector package. So far we have been extensively using lists. And though lists are ubiquitous in Haskell, they are not  efficient where array like access and operations are required. A vector supports array like O(1) access to elements, as well as list like incremental access. The vectors come in two flavors, immutable and mutable. We will look at both in this recipe.


How do we do it...
------------------

  <ul>
  <li> Create a new project *working-with-vector* with *simple* stack template.

~~~
  stack new working-with-vector simple
~~~

  <li>Add the dependency on the vector package in the build-depends subsection of the executable secton.

~~~
  executable working-with-vector
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , vector
~~~

  <li>Open src/Main.hs and start coding there. We will be experimenting with vector in this file.

> module Main where

  <li>Import both immutable and mutable vector modules

> import qualified Data.Vector as V
> import qualified Data.Vector.Mutable as MV
> import Data.Vector ((//),(!),(!?))

  <li>We will use smaller functions to demonstrate the vector and its abilities.

  <li> __Construction__ :

  We can construct immutable vectors either as empty, singleton, replicated, generated over input index, or combined with previous value. 

> constructVectors :: IO ()
> constructVectors = do
>   let e = V.empty :: V.Vector Int
>       s = V.singleton "one" :: V.Vector String
>       r = V.replicate 10 "same" :: V.Vector String
>       g = V.generate 10 (const "generated")  :: V.Vector String
>       i = V.iterateN 10 ('x':) "o"
>   putStrLn $ "Empty vector " ++ show e
>   putStrLn $ "Singleton vector " ++ show s
>   putStrLn $ "Replicated vector " ++ show r
>   putStrLn $ "Generated vector " ++ show g
>   putStrLn $ "Iterated vector " ++ show i

  <li> Construct vectors through enumeration. The function *enumFromTo* can also be used, but it is slower than *enumFromN*. 

> enumeratedVectors :: IO ()
> enumeratedVectors = do
>   putStrLn "Create a list of 10 floats, 1.1, 2.1 ... etc"
>   print $ (V.enumFromN 1.1 10 :: V.Vector Float)
>   putStrLn "Create a list of 10 floats, incremented by 0.5"
>   print $ (V.enumFromStepN 1.1 0.5 10 :: V.Vector Float)

  <li> __Vector as list__

  Vector supports many functions similar to *list*. Note that all operations are *O(1)*.

> vectorAsList :: IO ()
> vectorAsList = do
>   let vec = V.enumFromStepN 1 3 30 :: V.Vector Int
>   putStrLn "All elements but the last"
>   print $ V.init vec
>   putStrLn "Head of the vector"
>   print $ V.head vec
>   putStrLn "Tail of the vector"
>   print $ V.tail vec
>   putStrLn "Take first five elements"
>   print $ V.take 5 vec
>   putStrLn "Drop first five elements"
>   print $ V.drop 5 vec
>   putStrLn "Prepend and Append an element"
>   print $ V.cons 99 vec
>   print $ V.snoc vec 99
>   putStrLn "Concatenate two vectors"
>   print $ vec V.++ (V.fromList [101,102,103])

  <li> Bulk update of vectors.

> bulkOperations :: IO ()
> bulkOperations = do
>   putStrLn "Replace elements by list of index and value."
>   print $ (V.fromList [2,5,8]) // [(0,3),(1,6),(2,9)]
>   putStrLn "Update with another vector with index and value"
>   print $ (V.fromList [2,5,8]) `V.update` (V.fromList [(0,3),(1,6),(2,9)])

  <li> **Indexing operations** -

> indexing :: IO ()
> indexing = do
>   let vec = V.enumFromStepN 1.1 0.5 20
>   putStrLn "Input Vector"
>   print vec
>   putStrLn "Accessing 10 th element"
>   print $ vec ! 9
>   putStrLn "Safely accessing 10th element, and 100th one"
>   print $ vec !? 9
>   print $ vec !? 99

  <li> Creating mutable vector - Mutable vectors are created either in *IO* or *ST* monad. Here we create with *IOVector*

> mutableVec :: IO (MV.IOVector Int)
> mutableVec = do
>   v <- MV.new 2  -- Create a vector of size 2
>   MV.write v 0 1 -- Assign all values
>   MV.write v 1 2
>   return v

  <li> Use the mutable vector, and *freeze* it to convert to immutable vector.

> useMutable :: IO ()
> useMutable = do
>   mv <- mutableVec
>   vec <- V.freeze mv
>   putStrLn "Mutable to vector conversion"
>   print vec

  <li> Put all of above together

> main :: IO ()
> main = do
>  putStrLn "Constructing Vectors"
>  constructVectors
>  putStrLn "Enumerating Vectors"
>  enumeratedVectors
>  putStrLn "Vector as fast lists"
>  vectorAsList
>  putStrLn "Bulk operations on vector"
>  bulkOperations
>  putStrLn "Accessing elements of vector"
>  indexing
>  putStrLn "Working with mutable, and converting it to vector"
>  useMutable

  <li> Build and run the project

~~~
  stack build
  stack exec -- working-with-vector
~~~

  You should see following output -


  </ul>

How did we do it...
-------------------

As compared to *List*, *Map* and *Set*, the vector poses a very different approach. It is a very efficient collection, and most access operations are done with *O(1)* access. The vector is used in many efficient libraries such as *aeson* where the efficiency and random access is required. The vector itself is immutable and provides effective subsetting through list like operations.

The mutable vector on the other hand works throug monad, by allowing us to programmatically construct the vector, and then freezing it to convert it to immutable vector.
