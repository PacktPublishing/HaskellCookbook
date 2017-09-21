
Routing in Snap
===============

In this recipe, we will add *routes* using *Snap* framework. We will add plain routes and routes with parameters. 

How do we do it...
------------------
  <ul>

  <li> Create new project *routing-in-snap* with *simple* stack template.

~~~
  stack new routing-in-snap simple
~~~

  <li> Add dependency on following libraries in the *build-depends* sub-section of *executable* section.

~~~
  executable routing-in-snap
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , snap-server
                       , snap-core
                       , snap
                       , lens
                       , bytestring
                       , text
~~~

  Use following command to solve the dependency constraints to update the *stack.yaml* file.

~~~
  stack --resolver lts-9.1 solver --update-config
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add the imports for *Snap*.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Data.Monoid
> import Control.Applicative
> import Snap
> import Snap.Core
> import Snap.Http.Server

  <li> Add the routes. We will add two routes. The first route **hello** is where we will respond with a standard *Hello World!* greeting. The second route *greet/:nameparam* has a parameter embedded in the route. The parameter *nameparam* is embedded in the route path with a colon *:*.

> routes = [ ("hello", writeBS "Hello World!")
>          , ("greet/:nameparam", greetHandler)
>          ]

  <li> Next, we will add an handler for *greet/:nameparam* route. We access the named parameter with function *getParam*. This *may* fetch us the value of the parameter. We write an error message if the parameter value is not specified. 

> greetHandler = do
>   name <- getParam "nameparam"
>   maybe (writeBS "nameparam not specified") (\n -> writeBS ("Welcome " <>n)) name

  <li> Compose the routes in a single site. The top route and other routes are combined with *<|>* (instance of *Alternative* type class). 

> site = 
>   ifTop (writeBS "Serving from root") <|>
>   route routes

  <li> Use the *quickHttpServe* method to serve the site.

>
> main :: IO ()
> main = quickHttpServe site


  <li> Build and execute the project.

~~~
  stack build
  stack exec -- routing-in-snap
~~~

  The server will run at *0.0.0.0:8000*, connect to the local host by pointing browser to *http://localhost:8000/greet/snap*. You should see following message. 

  </ul>


How did we do it...
-------------------

In this recipe, we used a function *route* and *ifTop* to compose a set of paths to build the application. We also used part of the path as a parameter. The function quickHttpServe, runs the HTTP server at the port 8000. 

