
Working with Monad-Par
======================



How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-monad-par* with *simple* stack template.

~~~
  stack new working-with-monad-par simple
~~~

  <li> Add dependency on *monad-par* library in the *build-depends* sub-section of *executable* section. Also add *-threaded -fprof-auto -rtsopts -eventlog* to the *ghc-options* subsection.

~~~
  executable working-with-monad-par
    hs-source-dirs:      src
    main-is:             Main.hs
    ghc-options:         -threaded -fprof-auto -rtsopts -eventlog
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , monad-par
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. After the *Main* module, add relevant imports.

> module Main where
>
> import Control.Monad.Par
> import Data.Int
> 

  <li> Write simple *fibonacci* number calculator.

> fib :: Int64 -> Int64
> fib 0 = 0
> fib 1 = 1
> fib n = let x = fib (n-1)
>             y = fib (n-2)
>         in (x `seq` x) + (y `seq` y)

  <li> Now write a *parallel* fibonacci with threshold. This implementation is taken from http://www.cse.chalmers.se/edu/year/2015/course/pfp/lectures/lecture2/Marlow14.pdf 

> pfib :: Int64 -> Int64 -> Par Int64
> pfib n threshold | n <= threshold = return (fib n)
> pfib n threshold = do
>   n_1f <- spawn $ pfib (n-1) threshold
>   n_2f <- spawn $ pfib (n-2) threshold
>   n_1  <- get n_1f
>   n_2  <- get n_2f
>   return (n_1 + n_2)

  <li> Try parallel version, *pfib* in the *main* function.

> main :: IO ()
> main = do
>   putStrLn "Run pfib 30 with some threshold"
>   print $ runPar $ pfib 30 15


  <li> Build and execute the project, with 4 cores, and with *-l* option to produce event log. 

~~~
  stack build
  stack exec -- working-with-monad-par +RTS -N4 -l
~~~

  You should see following output,


  The threadscope should show the activity distributed all over the cores.

  </ul>


How did we do it...
-------------------

The *monad-par* defines *Par* monad which defines following primitives,

* *new* - Define a new empty *IVar*
* *get* - Wait for having some value in *IVar*
* *put* - Put some value into *IVar*
* *fork* - Signal that the input *Par* action can be run in parallel
* *spawn* - A function to run an action in parallel to produce an *IVar*

Internally, *monad-par* implements a way to balance these activities across threads by scheduling and balancing them. The *monad-par* allows us to specify parallel task without getting into nitty-gritties of actually scheduling tasks.

Also note that, in the above recipe, we have used a threshold to run the task sequentially, this is to increase the grannularity of the tasks. This achieves more speedup when there is no threshold. As when there is no threshold, the tasks become too small to have any advantage of parallelism. 


