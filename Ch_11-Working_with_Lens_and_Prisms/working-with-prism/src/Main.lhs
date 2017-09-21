Working with Prism
==================

*Lens* gives you an ability to focus on a particular field in a data type. *Traversal* will do the same thing for a traversable (something that you can traverse and collect). But these data types were *product* types. 

In this recipe, we will work with *Prism* where we will work with *sum type* data.


How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-prism* with *simple* stack template.

~~~
  stack new working-with-prism simple
~~~

  <li> Add dependency on *lens* library in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-prism
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , lens
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Enable *TemplateHaskell*, as we will produce *Prism* using template Haskell. Add *Main* module. Import *template haskell* module for lens, alongwith other imports.

> {-# LANGUAGE TemplateHaskell #-}
>
> module Main where
>
> import Control.Lens
> import Control.Lens.TH
> 

  <li> Define the sum data type.

> data Point = Point { _x :: Double, _y :: Double } deriving Show
> data Shape = Rectangle { _leftTop :: Point, _rightBottom :: Point }
>            | Circle { _centre :: Point, _radius :: Double }
>            deriving Show

  <li> Make the *lens* and *prism* out of it.

> makeLenses ''Point
> makeLenses ''Shape
> makePrisms ''Shape

  <li> Create some shapes

> makeRectangle :: Shape
> makeRectangle = Rectangle (Point 0 0) (Point 100 100)
>
> makeCircle :: Shape
> makeCircle = Circle (Point 0 0) 25.0

  <li> Work with *Shape* using prism and lenses

> usePrism :: IO ()
> usePrism = do
>   let rect = makeRectangle
>       circ = makeCircle
>   putStrLn "Create two shapes"
>   print rect
>   print circ
>
>   -- Check if we are dealing with rectangle
>   putStrLn "Are we dealing with a rectangle"
>   print $ rect ^? _Rectangle
>
>   putStrLn "Get first point of rectangle"
>   print $ rect ^? _Rectangle . _1
>
>   putStrLn "Get Y coordinate of the right bottom point of rectangle"
>   print $ rect ^? _Rectangle . _2 . y
>
>   putStrLn "Get the rectangle back from two points"
>   print $ _Rectangle # (Point (-1) (-1), Point 1 1)
>
>   putStrLn "Get radius of a circle"
>   print $ circ ^? _Circle . _2
>
>   putStrLn "Create circle from center and point"
>   print $ (Point 0 0, 10.0) ^. re _Circle
>
>   putStrLn "Change radius of the circle (from 25 to 3)"
>   print $ over (_Circle . _2) (const 3) circ
>
>   putStrLn "Get details of rectangle back from rectangle by traversing"
>   print $ rect ^.. _Rectangle
>   -- This is equivalent to toListOf
>   print $ toListOf _Rectangle rect
>
>   -- Create list of shapes
>   let shapes = [rect, circ]
>
>   putStrLn "Return result if all are rectangles"
>   print $ shapes ^.. below _Rectangle
>
>   putStrLn "Now try with all rectangles"
>   let rects = [rect, rect]
>   print $ rects ^.. below _Rectangle
> 
>   

  <li> Use the *lens*es and *prism*s in the main function.

> main :: IO ()
> main = usePrism

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-prism
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

*Prism* represents a traversal for a sum type. For a sum type, different alternatives are available for crating the data type. The *Prism* explores these alternatives. It also allows us to traverse to internals. Note the use of *_* for using *makePrisms* template. The generated prism will create a tupled representation (as we have sum of product types). One can combine *Prism* and *lens* as *Prism* is a valid traversal. Similarly, we can also use *over* to change the internals of a data.



