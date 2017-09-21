
Working with splice in Heist
============================

In previous recipe, we created a template and externally attached binding to a tag, and render the template using these bindings. This is useful when we have simple bindings. But what if, we have to do some calculation, and bind the calculation to the tag, rather than a simple string binding! In this recipe, we will be binding a tag to the local time on the server (or actually a place where the template will be rendered). 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-splice* with *simple* stack template.

~~~
  stack new working-with-splice simple
~~~

  <li> Add dependency on *heist* library in the *build-depends* sub-section of *executable* section. In addition, also add dependency on additional libraries required for this recipe. In addition to *text*, *bytestring*, *lens*, and *xmlhtml*, also add *time* library. 

~~~
  executable working-with-splice
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , heist
                       , text
                       , bytestring
                       , lens
                       , xmlhtml
                       , time
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add the support for *OverloadedStrings*. Also add required modules after the definition of *Main* module.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Heist
> import Heist.Interpreted
> import Data.ByteString.Char8 as B
> import Data.ByteString.Builder
> import Data.Text as T
> import Control.Lens
> import System.IO
> import qualified Text.XmlHtml as X
> import Data.Time
> import Control.Monad.IO.Class
> import System.Info

  <li> Create a splice for getting a time, and returning an node as a result of processing.

> currentTime :: MonadIO m => Splice m
> currentTime = do
>   formatnode <- getParamNode
>   let format = T.unpack $ X.nodeText formatnode
>   utc <- liftIO $ getCurrentTime
>   let ctime = formatTime defaultTimeLocale format utc
>   return [ X.TextNode $ T.pack ctime ]

  <li> Write a splice to get the os name and architecture.

> osSpecs :: MonadIO m => Splice m
> osSpecs = do
>   let specs = os ++ " : " ++ arch
>   return [ X.Element "em" [] [X.TextNode (T.pack specs)]]

  <li> Create a template *welcome.tpl* in the *templates* folder in the project directory. We will bind \<currentTime\/\> with the current time. 

~~~
<html>
  <body>
    <h1> Heist Framework </h1>

    <p> Welcome to Haskell built on <b> <osspec/> </b> </p>

    <p> This page binds two tags, viz.,  &lt;osspec&gt; and &lt;currenttime&gt; to the splices. The username is simply a text node bound to current OS architecture, whereas currenttime is bound to a splice that fetches the current time of the system, and formats it using the format string specified in the tag. 
    
    <p> This page was rendered on <b> <currenttime>%B %d, %Y</currenttime> </b>. </p>
  </body>
</html>
~~~

  <li> Write a function to load the templates.

> loadTemplateState :: IO (Either [String] (HeistState IO))
> loadTemplateState = do
>   -- Load all templates in the directory "templates"
>   loc <- loadTemplates "templates"
>   -- Create a config without a namespace
>   let ex  = over hcNamespace (const "") emptyHeistConfig
>       ex1 = over hcTemplateLocations (const [return loc]) ex
>   initHeist ex1


  <li> Bind the splice with correct tags.

> bindLocalSplices :: MonadIO m => HeistState m -> HeistState m
> bindLocalSplices =
>   bindSplice "osspec" osSpecs . bindSplice "currenttime" currentTime

  <li> Implement *main* function to load and run the *heist*.

> main :: IO ()
> main = do
>   Right st <- fmap bindLocalSplices <$> loadTemplateState
>   Just (b, mimeType) <- renderTemplate st "welcome"
>   hPutBuilder stdout b

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-splice
~~~

  You should see following output,

  
  </ul>


How did we do it...
-------------------

In this recipe, rather than binding to a static string, we created a splice (which is a monadic computation). We created a binding between a tag and the splice, and inserted this binding into a *HeistState*. The templates that are rendered in the given *HeistState* will dynamically apply these bindings and call the splice to create a resultant template.

It is thus possible to embed an arbitrary computation such as doing a database query, calling backend microservice etc, and populate the template using the splices. In this recipe, we bound a tag \<currenttime\/\>. The contents of this tag serves as an input to the splice. In this case, it is a time format in which the time should be output.
