
Working with Strategies
=======================

In this recipe, we will use *parallel* library. This library provides a set of strategies to allow us to program concurrent tasks easily. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-eval* with *simple* stack template.

~~~
  stack new working-with-eval simple
~~~

  <li> Add dependency on *parallel* library in the *build-depends* sub-section of *executable* section. Also add *-threaded* and *-fprof-auto -rtsopts -eventlog* for enabling multithreading and profiling.

~~~
  executable working-with-eval
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , parallel
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add *Main* module and import relevant modules.

> module Main where
>
> import Control.Parallel.Strategies

  <li> Create a data type for binary tree. 

> data BTree a = BTree (BTree a) a (BTree a) | Empty deriving Show

  <li> Add function to split a list in the middle. We wll partition the list in two two parts. A *Left* list, a *right* list and a *middle* element.

> split :: [a] -> ([a], Maybe a, [a])
> split xs     = split' xs xs []
>   where
>     split'  []       _        _   = ([], Nothing, [])
>     split'  (x:xs)  (_:[])    ls  = (reverse ls, Just x, xs)
>     split'  (x:xs)  (_:_:[])  ls  = (reverse ls, Just x, xs)
>     split'  (x:xs)  (_:_:ys)  ls  = split' xs ys (x:ls)

  <li> Now using a parallel strategy, build a balanced binary tree from the list.

> fromList :: [a] -> Eval (BTree a)
> fromList xs = do
>   (ls,m,rs) <- rseq (split xs)
>   ltree     <- rpar (runEval $ fromList ls)
>   rtree     <- rpar (runEval $ fromList rs)
>   case m of
>     Just x   -> return (BTree ltree x rtree)
>     Nothing  -> return Empty

  <li> Use the code in *main* function

> main :: IO ()
> main = do
>  let tree = runEval $ fromList [1..1000]
>  print tree


  <li> Build and execute the project. We run with a different runtime profiling output to generate event log. We use 4 cores (*-N2* options). You should feel free to modify this option to your hardware.

~~~
  stack build
  stack exec -- working-with-eval +RTS -N4 -l
~~~

  Typically, you should see a file *working-with-eval.eventlog*. If this file is not generated, (sometimes on windows, this behavior is observed), then you should run the executable directly by locating it in the *.stack-work* directory. 

  If you open the event log in the *threadscope* (https://wiki.haskell.org/ThreadScope), you should see following output. It should show all 4 cores being engaged. It also shows number of SPARKS generated. Though there is a scope for improvement (as you can still see gaps in the activity), this is a good start point for us.

  </ul>


How did we do it...
-------------------

There are two primitives, *rpar*, which immediately returns, and *rseq* which forces the argument into *WHNF or weak headed normal form*. The *WHNF* is related to lazy evaluation in Haskell, and usually points to a minimally evaluated *thunk*. In *WHNF*, only a part of the thunk is evaluated. Whereas, in *NF* or normal form, whole thunk is evaluated (or reduced).

This recipe, also shows how to profile the project, and look at the event log. This comes in very handy especially when dealing with concurrency.




