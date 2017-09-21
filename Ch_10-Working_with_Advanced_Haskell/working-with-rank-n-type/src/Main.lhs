Working with Rank-N-Type
========================

We will be using *ST s a* monad to convert imperative actions into pure actions.


How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-rank-n-type* with *simple* stack template.

~~~
  stack new working-with-rank-n-type simple
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> Add the language extension *Rank2Types*, and define *Main* module.

>
> module Main where
>

  <li> Import *ST* monad module.

> import Control.Monad.ST
> import Data.STRef
> import Control.Monad

  <li> Use the *ST* monad to calculate 

> factorialST :: (Num t, Eq t) => t -> STRef s t -> ST s t
> factorialST 0 x = readSTRef x
> factorialST n x = do
>   x' <- readSTRef x
>   writeSTRef x $! x' * n
>   factorialST (n-1) x

  <li> Convert the above factorial function from *ST s t* to pure function. 

> factorial n = runST $ do
>   x <- newSTRef 1
>   factorialST n x

  <li> Call the *factorial* in *main*.

> main :: IO ()
> main = do
>   putStr "100! = "
>   print $ factorial 100
>   putStrLn ""
>   putStr "500! = "
>   print $ factorial 500
>   putStrLn ""
> 

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-rank-n-type
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In this recipe, we use *ST s a*, which is defined as -

```
newtype ST s a = GHC.ST.ST (GHC.ST.STRep s a)
```

The *ST* is a special monad which lets us update in place. But it allows us to do this without explicit *IO*, by allowing us to escape *ST*. This is done by using *runST*. The function *runST* has a peculiar type - 

```
runST :: (forall s. ST s a) -> a
```

This is a rank 2 type, as the function *runST* must take an argument of type *ST s* and this argument should be universal in terms of *s*. Also note that the quantification (*forall s . ST s a*) helps *runST* escapes *ST*. 

In fact, all functions of rank 1, such as *map* can be used with quantifiers, for example,

```
map :: forall a b . (a -> b) -> [a] -> [b]
```

But with *runST* ,by specifying *(forall s . ST s a)*, we increase the rank to 2, because now we need one more quantifier. The actual type of *runST* is -

```
runST :: forall a . (forall s . ST s a) -> a
```

We stop at the rank-2 level, but you can imagine how we can move up the rank.
