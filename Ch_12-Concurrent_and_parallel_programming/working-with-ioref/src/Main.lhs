Working with IORef
==================

In this recipe, we will work with *IORef a*, mutable reference in *IO* monad. We will use *IORef Int* as a counter for the progress that can be tracked from a separate thread, while we do the *work* in the *main* thread.

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-ioref* with *simple* stack template.

~~~
  stack new working-with-ioref simple
~~~

  <li> Add the subsection *ghc-options* in the section *executable*. Add *-threaded* option for GHC compilation. If not provided, any foreign call will block all Haskell threads. Foreign calls are calls made outside Haskell runtime (typically by calling functions in external functions).

~~~
  executable working-with-type-family
    hs-source-dirs:      src
    main-is:             Main.hs
    ghc-options:         -threaded
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
~~~
  <li> Open *src/Main.hs*. We will be adding our source here. Define *Main* module.

> module Main where

  <li> Import module *Data.IORef* for *IORef*, and multithreading (*Control.Concurrent*).

> import Data.IORef
> import Control.Concurrent

  <li> Define the function *work*. The work involves atomically modifying *IORef* by adding *1* to the existing counter. Also use *threadDelay* to wait for half a second after each modification. Note that the function *threadDelay* expects time in microseconds. The *work* function is supplied a *count*. At every modification, the *work* function decreases the counter by *1*. The *work* terminates when the count goes down to zero. The *work* function returns the value of the previous count.

> work :: Int -> IORef Int -> IO Int
> work count i = work' count 0
>   where
>     work' count retval | count <= 0 = return retval
>     work' count _ = do
>       retval <- atomicModifyIORef' i (\j -> (j+1,j))
>       threadDelay (500*1000)
>       putStrLn $ "Work: Modifying progress to " ++ show retval
>       work' (count-1) retval

  <li> Write a tracker function. It takes a *IORef* and after every second (by using *threadDelay* as a waiting time inbetween) checks the value of the counter and prints it. 


> tracker :: IORef Int -> IO ()
> tracker i = do
>   threadDelay (1000*1000)
>   counter <- readIORef i
>   putStrLn $ "Tracker: Counting " ++ show counter
>   tracker i

  <li> In the *main* function, create a new *IORef* by using *newIORef* function. Use *forkIO* to create a new thread for the tracker. Do the work in the main thread, and once we are done with the work, we kill the tracker using *killThread* function.

> main :: IO ()
> main = do
>   counter <- newIORef 0
>   trackerId <- forkIO (tracker counter)
>   howmuch <- work 10 counter
>   killThread trackerId
>   putStrLn $ "Work completed with counter " ++ show howmuch

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-ioref
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

the *IORef a* represents a mutable reference in a IO Monad. In this recipe, we used *forkIO* run a *IO()* action for *tracker* in a seperate thread. The function *work* runs in the *main* thread. It uses *atomicModifyIORef'* to atomically increment the counter. This function will prevent race conditions for one *IORef*. Note that there are two versions of *atomicModifyIORef*. The version *atomicModifyIORef* is a lazy version (the operation done may not be evaluated immediately), whereas *atomicModifyIORef'* is a strict version. Finally, we kill the tracker thread with function *killThread*.



  
  
