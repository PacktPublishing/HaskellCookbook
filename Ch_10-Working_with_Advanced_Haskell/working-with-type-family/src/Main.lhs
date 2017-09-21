Working With Type Family
========================

In this recipe, we will work with *type family* or associated types. In *type family*, we associate a data type with another data type. In this particular recipe, we will work with a package *vector-space*, which beautifully show the association between types which defining *vector* and *scalar* types. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-type-family* with *simple* stack template.

~~~
  stack new working-with-type-family simple
~~~

  <li> Add dependency on *vector-space* library in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-type-family
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , vector-space
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> Define the module *Main*, and import *Data.VectorSpace*.

> module Main where
>
> import Data.VectorSpace
> 

  <li> Define vectors in three dimension by using a tuple.

> diagonal :: (Double, Double, Double)
> diagonal = (1, 1, 1)

> xaxis :: (Double, Double, Double)
> xaxis = (1, 0, 0)

  <li> Scale the diagonal vector, and take an inner product with *xaxis*.

> scaleanddotx :: Double -> (Double, Double, Double) -> (Double, Double, Double) -> Double
> scaleanddotx s p q = (s *^ p) <.> q

  <li> Print the values in *main* by invoking above functions.

> main :: IO ()
> main = do
>   putStrLn $ "Vector operation :- ((1,1,1) * 10) . (1,0,0)"
>   print $ scaleanddotx 10.0 diagonal xaxis
>   putStrLn $ "Midpoint of (1,1,1) and (1,0,0)"
>   print $ lerp diagonal xaxis 0.5

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-type-family
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------
The above example rather simple. But it cleverly uses the *type family*. Look at the definition of *VectorSpace* type-class. We know that we can scale a *vector* by a *scalar*. The type class *VectorSpace* allows us to define our data structure as a *vector*. But then we should also be able to customize which *scala* we would like to associate with this type.

The *VectorSpace* type class is defined as,

```
class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
 
```

In the above class definition, the type *Scalar* is defined as *type Scalar v :: \**. The type *Scalar v* means that for a data type *v*, which is an instance of *VectorSpace*, there is a scalar associated with it. And this type *Scalar* can be defined as we define the instance.

In the above recipe, we use *(Double, Double, Double)* as an instance of *VectorSpace*. This instance is defined as

```
instance ( VectorSpace u, s ~ Scalar u
         , VectorSpace v, s ~ Scalar v
         , VectorSpace w, s ~ Scalar w )
    => VectorSpace (u,v,w) where
  type Scalar (u,v,w) = Scalar u
```

This means that, if *u*, *v*, and *w* are vector spaces, and they share a common scalar value *s*. Then we can define *(u,v,w)* as an instance of *VectorSpace*, and its *Scalar* value is *s*.


