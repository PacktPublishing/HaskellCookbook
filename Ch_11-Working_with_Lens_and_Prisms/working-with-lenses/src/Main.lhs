Working with Lenses
===================

In this recipe, we will be working with *lens* library. This library provides a whole battery of functions. We will be using some of those functions. We will also create lenses for our own data type.

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-lenses* with *simple* stack template.

~~~
  stack new working-with-lenses simple
~~~

  <li> Add dependency on *lens* library in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-lenses
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , lens
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add *TemplateHaskell* extension for creating lenses for the user defined data types. Define *Main* module, and import necessary imports

> {-# LANGUAGE TemplateHaskell #-}
>
> module Main where
>
> import Control.Lens.TH
> import Control.Lens

  <li> Define data types *Line*, which is comprised of two end *Point*s. Note that we used *_* for naming the fields. 

> data Point a = Point { _x :: a, _y :: a } deriving Show
> data Line a = Line { _start :: Point a, _end :: Point a } deriving Show

  <li> Create lenses for *Point * and *Line*. We use *TemplateHaskell* support in *lens* to automatically create the lenses for *Point* and *Line*.

> makeLenses ''Point
> makeLenses ''Line

  This will remove the *underscores* from the field names, and make lenses out of it. 

  <li> Use the data types, and lenses in the *main* function.

> main :: IO ()
> main = do
>   let line = Line (Point 5 7) (Point 11 13)
>   putStrLn $ "Line " ++ show line
>   putStrLn $ "Using lenses"
>
>   -- Get the x coordinates of the start point
>   putStrLn "Start point of line"
>   print $ view start line
>   putStrLn "Composing lenses"
>   putStrLn "X of end of the line"
>   print $ view (end . x) line
>
>   putStrLn "Using setters"
>   putStrLn "Setting Y coordinate of end of the line"
>   print $ over (end . y) (const 17) line
>
>   putStrLn "Making it fancier with ^."
>   putStrLn "Access X of start of line"
>   print $ line ^. (start . x)

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-lenses
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In our last recipe, we have manufactured our own lenses. A lens is defined as,
```
type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
```

The creation of the lens is achieved via creation of *getter* and *setter* for a particular field in a data type.

```
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f x = fmap (setter x) $ f (getter x)
```

We have to write *getter* and *setter* manually for each field for supplying lenses. Using *TemplateHaskell* we can create these lenses. Also note the use of **_** while defining the data type.

