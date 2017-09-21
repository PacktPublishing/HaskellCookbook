Working with Existentially Quantified Type
==========================================

In this recipe, we will create a list of heterogeneous types which are instance of type class *Show*. We will use *ExistentialQuantification* extension to show the list. We will use *StandaloneDeriving* extension to derive a type class instance.


How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-existentials* with *simple* stack template.

~~~
  stack new working-with-existentials simple
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> Enable the existential support, and define the *Main* module. Also add *StandaloneDeriving* extension. 

> {-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
>
> module Main where

  <li> Define an existential type.

> data Display = forall a . Show a => Display a
> 

  <li> Create a standalone instance of *Show* for *Display*

> deriving instance Show Display 

  <li> Create a list of *Display*s.

> displayList :: [Display]
> displayList = [ Display 10
>               , Display ["One","Two","Three"]
>               , Display 10.0
>               , Display (Just "Something")
>               , Display True ]

> main :: IO ()
> main = do
>   putStrLn "Printing haterogenous showable list"
>   print displayList

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-existentials
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------
In this recipe, we have primarily used *ExistentialQuantification* extension. Note how we have defined the data type, *Display*

```
data Display = forall a . Show a => Display a
```

An important thing to note here is that the type variable *a* does not appear on the left hand side. It only appears on right hand side expressions. it also appears with a construct *forall a . Show a*. It is a way of embedding an information about type class that is embedded inside the data type. In this case, it tells us that *Display* is defined for all *a* which are instances of *Show*.

In this way, the data constructor *Display a* embeds a value of type *a*. But the only information that is available with us is about the type class *Show*. It means that we can only call functions of type class *Show* here. This is how we could encode, an *Int*, *List*, *Maybe* etc. in the same list in the above example and *show* it too!

We have also used another extension called *StandaloneDeriving*. Here because, *Display* is an existential type, we cannot say *deriving Show* for *Display*. However, we can create a standalone deriving instance by saying *deriving instance Show Display* (without the *where* clause). 

The standalone deriving instance can be defined in another file as well. Also note that the standalone deriving instance can be for specific data type (or constraint) as well.
