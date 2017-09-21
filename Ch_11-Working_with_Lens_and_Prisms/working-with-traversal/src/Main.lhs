
Working with Traversal
======================

In this recipe, we will work with traversals, where we can use lens for traversing many fields. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-traversal* with *simple* stack template.

~~~
  stack new working-with-traversal simple
~~~

  <li> Add dependency on *lens* library in the *build-depends* sub-section of *executable* section. Also add *containers* as we will be using *Map* in this recipe.

~~~
  executable working-with-traversal
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , containers
                       , lens
~~~

  <li> Open *src/Main.hs*. We will be editing this file for this recipe. Add the module definition for *Main*. Also import required modules. Enable *TemplateHaskell* at the top for we will be creating lenses using template Haskell.

> {-# LANGUAGE TemplateHaskell #-}
> module Main where
>
> import Data.Map
> import Control.Lens
> import Control.Lens.TH

  <li> Add data type for maintaining list of exchanges and symbols. We keep symbols as a map between unique symbol ID and generic symbol name. 

> data Symbol = Symbol { _sid :: String, _sname :: String } deriving Show
> type Symbols = Map String Symbol
> data Exchange = Exchange { _exchange :: String, _symbols :: Symbols } deriving Show

  <li> Make the lenses for above data types.

> makeLenses ''Symbol
> makeLenses ''Exchange

  <li> Populate symbols from two exchanges, viz., Singapore and National Stock Exchange, India.

> singExchange :: Exchange
> singExchange =
>   let symbols = [ Symbol "1A1" "AGV Group Ltd"
>                 , Symbol "D05" "DBS Group Holding"
>                 , Symbol "CC3" "StarHub Ltd." ]
>   in Exchange "SGX" $ fromList $ zip (_sid <$> symbols) symbols

> nseExchange :: Exchange
> nseExchange =
>   let symbols = [ Symbol "3MINDIA" "3M India Ltd"
>                 , Symbol "HINDALCO" "Hindalco Industries Ltd"
>                 , Symbol "HCLTECH" "HCL Technologies Ltd" ]
>   in Exchange "NSE" $ fromList $ zip (_sid <$> symbols) symbols

> exchanges :: [Exchange]
> exchanges = [singExchange, nseExchange]

  <li> Use the above data types in the traversals.

> main :: IO ()
> main = do
>   putStrLn $ "Just traverse the exchanges, should get back same input"
>   print $ toListOf traverse exchanges
>
>   putStrLn $ "Traverse and modify names of the exchanges, prepend 'X' to the exchange names"
>   putStrLn $ "Traversal is a valid lens, and can be combined with other lenses"
>   print $ over (traverse . exchange) ('X':) exchanges
>
>   putStrLn $ "Traverse and get combined list symbols across exchanges"
>   print $ view (traverse . symbols) exchanges
>
>   putStrLn $ "Get all symbol IDs in all exchanges"
>   print $ toListOf (traverse . symbols . traverse . sid ) exchanges
>
>   putStrLn $ "Same as above but with 'view' rather than 'toListOf'"
>   print $ view (traverse .symbols . traverse . sid ) exchanges
>
>   putStrLn $ "Use 'set' to set everything in the traversal to the same value"
>   print $ set traverse 8 [1..10]

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-traversal
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

We have seen *lens* in last recipe. A lens applies to a field in a data structure. If we have a *Traversable* field type somewhere, then we can use traverse to browse over the collection. An important fact must be remembered, if we view the traverse, then the target field must be an instance of *Monoid*. For example, *toListOf traverse exchanges* will be successful, and will fetch same list of exchanges back. But *view traverse exchanges* will fail with a complain that *Exchange is not an instance of Monoid*. This happens because, traversal tries to summarize the target values by assuming that the target type is an instance of *Monoid*. Hence it starts with empty value (*mempty*) and then starts appending values *mappend/mconcat*.

It is also possible combine traversal with lenses. In fact, to get to the list of all symbols we used following lens,

```
traverse . symbols . traverse . sid
```

Here first traverse applies to list of exchanges, *[Exchange]*, the second element in the above composition *symbols* will lead us to symbol *Map*. Since *Map* is an instance of *Traversable*, we can also use *traverse* to browse throught the map values. This is where third element *traverse* comes from. In the end, we are interested in symbol ID of each symbol. Hence, the last element in the above composition is *sid*. Thus you can see that traversals can be easily composed with lenses. (You should read the above composition from left to right).

Since traversal is a valid lens, we can also use *over* to set or modify the value in the data structure. If we would like to change all the fields visited to a single value then we can use *set* (as we have used above).
