<ul>

  <li> Enable *OverloadedStrings*

> {-# LANGUAGE OverloadedStrings #-}
>

  <li> Add necessary imports

> 
> import Prelude
> import qualified Prelude as P
> import Data.Monoid (mempty,(<>))
> 
> import Text.Blaze.Html5 hiding (main)
> import qualified Text.Blaze.Html5 as H
> import Text.Blaze.Html5.Attributes
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
> import qualified Data.ByteString.Lazy.Char8 as BC

  <li> Define shoarthand for often used URLs

> -- Attribute value
> -- Point to CDNJS library
> cdnjs :: AttributeValue
> cdnjs = "https://cdnjs.cloudflare.com/ajax/libs/"
> 
> -- Bootstrap 4.0 base for example
> bootstrapUrl :: AttributeValue
> bootstrapUrl = "http://getbootstrap.com/docs/4.0/"

  <li> We will be creating the navigation bar example from bootstrap. Create the header for our purpose

> -- Create Header 
> navHeader :: Html
> navHeader = H.head $ do
>   meta ! charset "utf-8"
>   meta ! name "viewport" ! content "width=device-width, initial-scale=1, shrink-to-fit=no"
>   meta ! name "description" ! content "Navigator example from Bootstrap"
>   meta ! name "author" ! content "Haskell Cookbook"
>   H.title "Top navbar example for Bootstrap Using Blaze-Html"
>   link ! href (cdnjs <> "twitter-bootstrap/4.0.0-beta/css/bootstrap.min.css") ! rel "stylesheet"
>   --  Point to bootstrap example css
>   link ! href (bootstrapUrl <> "examples/navbar-top/navbar-top.css") ! rel "stylesheet"


  <li> Now create a *body* with navigation bar and the container pointing to some text.

> navBody :: Html
> navBody = body $ do
>   -- Create navigator bar
>   nav ! class_ "navbar navbar-expand-md navbar-dark bg-dark mb-4" $ do
>     a ! class_ "navbar-brand" ! href "#" $ "Top navbar"
>     button ! class_ "navbar-toggler" ! type_ "button" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" "#navbarCollapse" $ H.span ! class_ "navbar-toggler-icon" $ mempty
>     H.div ! class_ "collapse navbar-collapse" ! A.id "navbarCollapse" $ do
>       ul ! class_ "navbar-nav mr-auto" $ do
>         li ! class_ "nav-item active" $ a ! class_ "nav-link" ! href "#" $ do
>           "Home"
>           H.span ! class_ "sr-only" $ "(current)"
>         li ! class_ "nav-item" $ a ! class_ "nav-link" ! href "http://www.haskell.org" $ "Haskell"
>         li ! class_ "nav-item" $ a ! class_ "nav-link disabled" ! href "#" $ "Disabled"
>       H.form ! class_ "form-inline mt-2 mt-md-0" $ do
>         input ! class_ "form-control mr-sm-2" ! type_ "text" ! placeholder "Search"
>         button ! class_ "btn btn-outline-success my-2 my-sm-0" ! type_ "submit" $ "Search"
> 
>   H.div ! class_ "container" $ H.div ! class_ "jumbotron" $ do
>     h1 $ do
>       "Navbar example using "
>       b $ "blaze-html"
>     p ! class_ "lead" $ do
>       "This example shows how to use blaze-html with bootstrap framework using "
>       i $ "Text.Blaze.Html5"
>       " and bootstrap defined classes and tags"
>     a ! class_ "btn btn-lg btn-primary" ! href ( bootstrapUrl <> "components/navbar/") $ "View navbar docs"

  <li> Combine the header and body with scripts in *Html5* DSL

> index :: Html
> index = docTypeHtml ! lang "en" $ do
>   navHeader
>   navBody
>   --  Bootstrap core JavaScript
>   --     ================================================== 
>   --  Placed at the end of the document so the pages load faster 
>   script ! src "https://code.jquery.com/jquery-3.2.1.slim.min.js" $ mempty
>   script "window.jQuery || document.write('<script src=\"https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js\"><\\/script>')"
>   script ! src (cdnjs <> "popper.js/1.12.3/esm/popper.min.js") $ mempty
>   script ! src (cdnjs <> "twitter-bootstrap/4.0.0-beta/js/bootstrap.min.js") $ mempty
> 

  <li> Use a *ByteString* to render the html from *index* function. 

> main :: IO ()
> main = BC.putStr $ renderHtml index
> 

