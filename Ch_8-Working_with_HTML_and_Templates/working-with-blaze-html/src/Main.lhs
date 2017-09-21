Use blaze to create HTML template
=================================

In this recipe, we will be using *blaze-html* library to construct HTML documents. The *blaze-html* library provides very efficient and fast DSL for constructing HTML documents. It is very lightweight, and supports efficient UNICODE support. Being embedded inside Haskell, one can take full advantage of *Haskell* while constructing HTML documents. It also supports HTML5 and HTML4 strict syntax.

Note that the aim of the recipe is not to showcase HTML, but the interoperability between Haskell and HTML through *blaze-html*. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-blaze-html* with *simple* stack template.

~~~
  stack new working-with-blaze-html simple
~~~

  <li> Add dependency on blaze-html library in the *build-depends* sub-section of *executable* section.

~~~
  executable blaze-html
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , blaze-html
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> Add the extension *OverloadedStrings*, this will enable us to work with many strings that we will use in this recipe. Also add *Main* module definition

> {-# LANGUAGE OverloadedStrings #-}
> module Main where


  <li> Import *blaze-html* modules for creating HTML5 elements and attributes

> import Control.Monad
> import Text.Blaze.Html5 as H hiding (main)
> import Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Html.Renderer.Pretty (renderHtml)

  <li> Create data type to represent a user and his interest in equity stocks!

> data User = User { firstName :: String, lastName :: String } deriving Show
> data Stock = Stock { symbol :: String, exchange :: String, description :: String } deriving Show
> data UserStocks = UserStocks { user :: User, stocks :: [Stock] } deriving Show

  <li> Create some data

> sampleUser :: User
> sampleUser = User "Jerry" "McQuire"
>
> sampleStocks :: [Stock]
> sampleStocks = [ Stock "D05" "SGX" "DBS Group"
>                , Stock "GOOGL" "NASDAQ" "Alphabet Inc"
>                , Stock "INFY" "BSE" "Infosys Ltd"
>                ]
>
> sampleData :: UserStocks
> sampleData = UserStocks sampleUser sampleStocks
> 

  <li> Use the *blaze* HTML5 primitives to create an HTML. Use a CSS on cloud for styling. The user's stocks are reprented by a table.

> sampleHtml (UserStocks user stocks) = html $ do
>   header $ do
>     H.title $ toHtml $ "Stock Data for " ++ lastName user ++ ", " ++ firstName user
>     link ! rel "stylesheet" ! type_ "text/css" ! href "https://cdnjs.cloudflare.com/ajax/libs/aegis/1.3.3/aegis.css"
>   body $ do
>     h1 $ toHtml $ "Stock Data for " ++ lastName user ++ ", " ++ firstName user
>     p $ table $ do
>       thead $ do
>         th $ H.span $ toHtml ("Stock"::String)
>         th $ H.span $ toHtml ("Exchange"::String)
>         th $ H.span $ toHtml ("Description"::String)
>       forM_ stocks $ \s -> do
>         tr $ do
>           td $ toHtml $ symbol s
>           td $ toHtml $ exchange s
>           td $ toHtml $ description s
>       

  <li> Use *main* to create a HTML page.

> main :: IO ()
> main = do
>   putStr $ renderHtml $ sampleHtml sampleData

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-blaze-html  > example.html
~~~

  If you open example.html in the browser, you should see following HTML page.


  </ul>


How did we do it...
-------------------

The library *blaze-html* is derived from *blaze*, an amazingly fast text builder library which constructs text data in chunks. The library *blaze-html* provides a DSL for representing HTML. In fact, it provides three HTML DSLs, HTML5, HTML4 (Strict and Transitional), and XHTML.

All of them offer a monadic way of combining different HTML elements and creating an HTML as a structure represented by data type *Html* (an alias of *Markup*). Once created, one can *render* the *Html* element using different renderers (such as Pretty, String, Text and Utf8).

The *Html* data type itself is an alias of *Text.Blaze.Internal.Markup*. Using *Markup*, it is possible to create custom HTML elements.

One of the important thing to note in the recipe is how smoothly we can combine user's data type into templates. We can create composable functions to render an *Html* element from the given data structure.



