Understanding Lenses
=====================

If you have worked with a object oriented programming, then you must be aware of the properties (such as in *C#* or *Python*, or even in managed *C++*). Usually we can access the properties inside an object and also set the property to some value.

```
Point point = Point(1.0, 2.0);
double x = point.x; // Should be 1.0
point.x = 3.0;      // Now point x is changed to 3.0
```

Though the above code mutates the data, it is very convenient to get and set a property. Imagine doing the same with *Haskell*.

```
data Point = Point Double Double

x :: Point -> Double
x (Point xv _) = xv

setx :: Point -> Double -> Point
setx (Point _ y) x = Point x y
```
We need to de-construct a type, and reconstruct it again. If we had some generic way of accessing a field inside the data, and then accessing it back, then we will get the lost convenience of getting and setting a property back. In this recipe, we will see how we can define a generic property getter and setter. 


How do we do it...
------------------
  <ul>

  <li> Create new project *creating-lenses* with *simple* stack template.

~~~
  stack new creating-lenses simple
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Define the *Main* module. Enable extension *Rank2Types* before the *Main* module.

> {-# LANGUAGE Rank2Types, StandaloneDeriving, DeriveFunctor #-}
> module Main where
>

  <li> Define a data type *Point*, which represents a two dimensional point.

> data Point = Point Double Double deriving Show
>

  <li> Now define imagine a generic structure *s*, and we need to get a field of type *a* from the structure. Its type would be *s -> a*. Now imagine we need to change some property of structure *s* with value *b*, and a result will be another structure *t*. This will have a type *s -> b -> t* (Actually, this a generic type for the specific setter *s -> b -> s*). Let's now define types for these getter and setter functions.

> type Getter s a = s -> a
> type Setter s b t = s -> b -> t

  <li> We will combine getter and settter in one type, *Lens*

> type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

  The above type is a *rank* 2 type, as it should work for any *Functor f*, and *f* is not included in the left hand side. Note that *s* is an input data type, *t* is the output data type. *a* corresponds to the some property of *s*, whereas *b* is some property type associated with *t*.

  <li> Let's now see if we can combine getter and setter to create a combined lens.

> lens :: Getter s a -> Setter s b t -> Lens s t a b
> lens getter setter f x = fmap (setter x) $ f $ getter x

  <li> Now define lenses for *x* and *y* coordinates in the *Point*. Since in our case, we would like to have output data type to be same as input data type, with same property type, we can define *Lens'* to be a restricted version of *Lens*

>
> type Lens' s a = Lens s s a a
>
> x :: Lens' Point Double
> x = lens getter setter
>   where
>     getter (Point xv _) = xv
>     setter (Point _ yv) xv = Point xv yv
>
> y :: Lens' Point Double
> y = lens getter setter
>   where
>     getter (Point _ yv) = yv
>     setter (Point xv _) yv = Point xv yv
> 

  <li> define an identity functor.

> newtype Access a s = Access { access :: a } deriving Show

  <li> Define instance of *Functor*

> deriving instance Functor (Access a)

  <li> Define a function to get field, given a lens

> view :: Lens' s a -> s -> a
> view l = access . l Access

  <li> Similarly define a generic function to set a field.

> newtype Binder a = Binder { bound :: a }
>
> deriving instance Functor Binder

> set :: Lens' s a -> a -> s -> s
> set l d = bound . l (const (Binder d))

  <li> Use the above lenses in the *main* function.

> main :: IO ()
> main = do
>   -- Create a point
>   let p = Point 3 5
>   putStrLn $ "Initial Point = " ++ show p
>   putStrLn $ "Getting x and y coordinates using lenses x and y"
>   print $ view x p
>   print $ view y p
>   putStrLn $ "Setting x and y coordinates alternatively using lenses x and y"
>   print $ set x 7 p
>   print $ set y 7 p

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- creating-lenses
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In this recipe, we have defined a generic lens *Lens s t a b* which expands to

```
fmap (setter x) $ f (getter x)
```

Here, *x* is some structure, *getter x* gets a field from x, and *setter x* is a function which takes and argument, and sets the same field, and returns a modified *x*. In the lens, they are connected together by *fmap*, with *f* transforming the value of the field to a *Functor*.

Since, the lens is a *rank 2* type, the choice of *f*, if made wisely, we can achieve both generic *get* and *set* at the same time. Thus, when creating a generic getter *view* funciton, we use *Access* as a *Functor*. The *Access* captures the field value, and ignores *fmap* mapping. On the other hand, the setter *set* function, uses *Binder* which takes the input field value, and submits itself to the *fmap* by replacing the field value in the given input *x*.

