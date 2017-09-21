
Form handling in Snap
=====================

In this recipe, we will look at how forms can be handled in Snap framework. We will also look at HTTP redirection, and handling GET and POST methods. 

How do we do it...
------------------
  <ul>

  <li> Create new project *form-handling-in-snap* with *simple* stack template.

~~~
  stack --resolver lts-9.1 new form-handling-in-snap simple
~~~

  <li> Add dependency on following libraries in the *build-depends* sub-section of *executable* section.

~~~
  executable form-handling-in-snap
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
                       , containers
~~~

  Once the dependency is added, solve the dependency constraints by using same resolver, and allowing *stack* to update the *stack.yaml* file.

~~~
  stack --resolver lts-9.1 solver --update-config
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. After the *Main* module header, add necessary imports. Also enable *OverloadedStrings* extension.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Data.Monoid
> import Control.Applicative
> import Snap
> import Snap.Core
> import Snap.Http.Server
> import Snap.Util.FileServe
> import Data.Map.Lazy as M
> import qualified Data.ByteString.Char8 as BC

  <li> Create a folder *static* in the project directory, and *form.html* to it with following contents. The following HTML document shows a form where a user can enter his/her first and last name, and his/her favorite Haskell web framework to work with. 

~~~
<!DOCTYPE HTML5>
<html>
  <body>
    <p> The form shown below takes the the input, and submits it to the action defined in Snap. Snap processes the action, and produces a page showing the processed input. </p>

    <form action="/survey">
      <fieldset>
        First Name : 
        <input type="text" name="firstname"><br>
        Last Name : 
        <input type="text" name="lastname"><br>
        Your favorite Haskell Web Framework 
        <select name="framework">
          <option value="snap">Snap Framework</option>
          <option value="yesod">Yesod Framework</option>
          <option value="happstack">Happstack </option>
        </select>
        <br><hr>
        <input type="submit" value="Complete Survey">
      </fieldset>
    </form>
  </body>
</html>
~~~

  We will serve this directory with *static* as a root folder.

  <li> Next, add a handler for GET method. We will redirect to the form defined above.

> getSurvey :: MonadSnap m => m a
> getSurvey = method GET (redirect "/form.html")

  <li> Also add a handler for handling POST method for the form. We will grab the contents of the form, and write them as text. 

> postSurvey :: MonadSnap m => m ()
> postSurvey = method POST $ do
>   rq <- getRequest
>   params <- getParams
>   let fullName = extractName params
>   let favorite = extractFavorite params
>
>   maybe (writeBS "Hello Anonymous") (\n -> writeBS ("Hello " <> n)) fullName
>   maybe (writeBS "No preference") (\n -> writeBS ("Your favorite framework : " <> n)) favorite
>
>   where
>     extractName :: Params -> Maybe BC.ByteString
>     extractName params = do
>       firstname <- M.lookup "firstname" params
>       lastname  <- M.lookup "lastname" params
>       return $ (head firstname) <> " " <> (head lastname) <> "\n"
>
>     extractFavorite = fmap head . M.lookup "framework" 

  <li> Create routes for handling survey. Add separate route for handling GET and POST methods

> routes = [ ("/survey", postSurvey)
>          , ("/survey", getSurvey) ]

  <li> Create a site for combined static and survey handler

> site = route routes <|> serveDirectoryWith fancyDirectoryConfig "static"

  <li> Start the HTTP server with above site

> main :: IO ()
> main = quickHttpServe site
> 

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- form-handling-in-snap
~~~

  Point the browser to http://localhost:8000/survey. You should see following form,

  After submission, the output will show the selection done through the form. 

  </ul>


How did we do it...
-------------------

In this recipe, we have used following functionality,

* Separate routes for handling GET and POST methods.
* Using *getParams* to get the parameters associated with the *request*
* The GET method does the redirection to another location. This is achieved using function *redirect*.

