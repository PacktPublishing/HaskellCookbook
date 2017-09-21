
Creating and Composing Snaplets
===============================

In this recipe, we will create a snaplet, and build the Snap application around it. We will also use *heist* snaplet for serving the HTML templates. This recipe will demonstrate,

* How to create a snaplet, 
* How to use existing snaplet inside an existing snaplet, 
* How the snaplet data is structured and placed, and 
* How to access snaplet data.

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-snaplets* with *simple* stack template.

~~~
  stack new working-with-snaplets simple
~~~

  <li> Add dependency on snap-core library in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-snaplets
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , snap-core
                       , snap-server
                       , snap
                       , lens
                       , bytestring
                       , text
                       , mtl
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. After the initial *Main* module definition, add the necessary imports. Enable *OverloadedStrings* and *TemplateHaskell* extensions as Snap uses *Lens* Template Haskell library. 

> {-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
> module Main where
> 
> import Control.Applicative
> import Control.Lens
> import Control.Lens.TH
> import Control.Monad.State.Class (gets)
> import Data.ByteString.Char8
> import Data.Maybe
> import Data.Monoid
> import Snap
> import Snap.Core
> import Snap.Http.Server
> import Snap.Snaplet.Session
> import Snap.Snaplet.Heist

  <li> Create a data type *MyData*. It contains a list of *ByteString*.

> data MyData = MyData { _someData :: [ByteString] }

  <li> Create lenses for our data type. Note that lenses are covered in details in *Chapter 11*. In the context of this recipe, it is sufficient to know that while creating lenses, template haskell will remove an *_* (underscore) from the record field, and will create lens. In the above type *MyData*, a lens called *someData* will be created

> makeLenses ''MyData

  <li> Create a *Snaplet* for *MyData*. We create a snaplet that can be used in other Snap applications. 

> -- Initialize the snaplet 
> myDataInit :: SnapletInit b MyData
> myDataInit = makeSnaplet "myData" "Snaplet with MyData" Nothing $ do
>   return (MyData ["My Data is initialized"])

  <li> Create the application that is composed of *Heist* snaplet and *MyData* snaplet.

> data MyApp = MyApp { _heist :: Snaplet (Heist MyApp)
>                    , _myData :: Snaplet MyData
>                    }

  <li> Create lenses for *MyApp*.

> makeLenses ''MyApp

  <li> Create a Snap handler function *snapletName*, which will access the current snaplet name, and will print it as a text.

> snapletName :: Handler b MyData ()
> snapletName = method GET $ do
>   name <- getSnapletName
>   let snapletname = fromMaybe "Cannot get snaplet name" name
>   writeText $ "Name of the snaplet : " <> snapletname

  <li> Create a Snap handler function *snapletData*, which will access the data stored in *MyData*, and print it as a text. 

> snapletData :: Handler b MyData ()
> snapletData = method GET $ do
>   mydata <- gets _someData
>   writeBS $ mconcat mydata

  <li> Now create the snaplet for *MyApp*. This snaplet will initialize the *heist* and *MyData* snaplets, and will also add routes for getting the name of the snaplet and accessing the data inside *MyData* snaplet.. It will also allow static serving of templates through *heist*.

> myAppInit :: SnapletInit MyApp MyApp
> myAppInit = makeSnaplet "myApp" "My First Snaplet" Nothing $ do
>   hst <- nestSnaplet "heist" heist $ heistInit "templates"
>   myd <- nestSnaplet "mydata" myData $ myDataInit
>   addRoutes [ ("/mysnaplet", with myData snapletName)
>             , ("/mysnaplet/data", with myData snapletData)
>             ]
>   wrapSite (<|> heistServe)
>   return (MyApp hst myd)

  <li> Create an instance of *HasHeist* type class. This will simplify accessing *heist* for binding templates etc. 

> instance HasHeist MyApp where
>   heistLens = subSnaplet heist

  <li> Use the *MyApp* snaplet to be served as the web application.

> main :: IO ()
> main = serveSnaplet defaultConfig myAppInit

  <li> We will still need to add some templates for *Heist*. Create a directory *snaplets* in the project directory, and create *heist/templates* subpath inside *snaplets* directory.

  <li> Add default template in the *snaplets/heist/templates* directory

~~~
<html>
  <head>
    <title>Creating and composing Snaplets</title>
  </head>

  <apply-content/>
</html>
~~~

  <li> Add index template in the same *templates* directory. The index template uses *default* demplate.

~~~
<apply template="default">
  <h1> Welcome to Heist </h1>
  <p>
    This page is displayed through <em>Heist</em> snaplet.
  </p>
</apply>
~~~

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-snaplets
~~~

  <li> The Snap server will serve at the port 8000. Point browser to http://localhost:8000, you should see following HTML output.

  <li> If you enter http://localhost:8000/mysnaplet, you should see the name of the snaplet. The output should look like following.

  <li> You can see the following output when you load the URL http://localhost:8000/mysnaplet/data

  </ul>


How it works...
-------------------

In this recipe, we create two snaplets, for two data types, one for *MyData*, and another for *MyApp*. The list below shows important aspects of creating and composing snaplets.

* The *MyData* contains a list of strings. the field *_someData* starts with an underscore. This is done so that *makeLenses* (a Haskell Template function) can generate *lens* for accessing and setting the field. More information about lenses is covered in Chapter 11. Note that, in this recipe we do not use *lens* as such, and use of lenses is strictly restricted only where Snap apis demand. 

* The *makeSnaplet* function takes the snaplet name, description, and an optional data directory on the disc. Here we have used default option (*Nothing*) for the data directory. The *makeSnaplet* function takes the initializer function. In the case of *MyData*, we have written *myDataInit*, which creates the initial data for *MyData*.

* The data type *MyApp* represents the web application we are building. It has two fields, one points to *Snaplet Heist*, and another points to *Snaplet MyData*. This is how, snaplets can be composed together. Also note that the names of the record fields in this data type start with underscore so that *lens* can be generated for each field.

* In the initializer *myAppInit* for *MyApp*, notice following things,

** The function *nestSnaplet* is used for initializing and nesting snaplets.
** The nesting is done with following syntax
~~~
nestSnaplet "mydata" myData $ myDataInit
~~~
   Here, the first parameter "mydata* is the name given for *instance* of the snaplet. The second parameter *myData* is the *lens* generated for *MyData* for *MyApp* field *_myData*, and *myDataInit* is the snaplet initialization function for *MyData* snaplet.

* In the *myAppInit* function, we use *addRoutes* to add routes for the *MyApp* snaplet. Each snaplet can have their own set of routes.

* We use *wrapSite (<|> heistServe)* in the *myAppInit* function. This is used for writing some initializer which has to be called before the site is served. Here we use *heistServe* to serve *heist* templates.

* In the handler *snapletName* and *snapletData* notice the signature of the function. The following is the type signature for these handlers.
~~~
Handler b MyData ()
~~~
  In the signature *b* is the snaplet inside which we are working. In the current recipe, it will be *MyApp*. The second type parameter *MyData* denotes the current Snaplet data type. The third parameter *()* is the return type of Monad *Handler b MyData*.

* Handlers in Snap are *State* monad, and it is possible to use *gets*, *puts* to be used inside the *Handler*. Here we use *getSnapletName* to get the name of the snaplet, and *gets* to get the data *_someData* inside *MyData*.

* Each snaplet can expect the data in the path *snaplets/<snapletname>*. In the current recipe, *heist* data is located in *snaplets/heist* directory. *Heist* has a convention to put all templates in the *templates* sub-directory in its assigned path.

* In the template, we use use two templates *default* and *index*. Notice that the *index* template only defines the specific data for index page, whereas *default* represents a generic HTML structure. The *apply-content* tag in *default* template embeds contents of *apply* tag in *index* templates to serve the *index* template. 
