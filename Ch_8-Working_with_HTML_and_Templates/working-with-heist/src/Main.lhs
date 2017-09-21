
Working with Heist
==================

In this recipe, we will work with *heist*, a templating framework that can work with HTML or XML documents. The *heist* framework is also a default templating framework used for *Snap* web development framework. At the same time, *heist* does not have any dependency on *Snap* and can be used independently.

In this recipe, we will create a template, and use *bind* to bind a value to the template. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-heist* with *simple* stack template.

~~~
  stack new working-with-heist simple
~~~

  <li> Add dependency on *heist* library in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-heist
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , heist
~~~

  The library *heist* does not exist in the stackage LTS. Hence we need to run following command to update the dependencies automatically.

~~~
  stack solver --update-config
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Enable *OverloadedStrings* extension, and define *Main* module.

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

  <li> Now we will create few heist templates. Heist templates are XML documents. Create a subdirectory *templates* in the project folder. Create a file *HelloWorld.tpl*. (*tpl* extension stands for template).

~~~
<h1> Hello <name/> </h1>
<p>
  In this example, we will look at templates and bindings. You, <familyname/>, <name/> will be creating some templates, and then using these templates to generate something wonderful.

  In heist, you can always bind with a tag.
</p>
~~~

  Note the use of \<name\/\> and \<familyname\/\>. These are the parameters which are filled at runtime.

  <li> Create a configuration for loading the templates. We do not use any namespace, and hence initialize *Heist* configuration with empty namespace. Load the templates from the "templates" directories. Set the loaded templates in the configuration. The name of the template file will serve as the name of the template. In this case "HelloWorld" will be the name of the template.

> loadTemplateState :: IO (Either [String] (HeistState IO))
> loadTemplateState = do
>   -- Load all templates in the directory "templates"
>   loc <- loadTemplates "templates"
>   -- Create a config without a namespace
>   let ex  = over hcNamespace (const "") emptyHeistConfig
>       ex1 = over hcTemplateLocations (const [return loc]) ex
>   initHeist ex1

  <li> *HeistT* is a monad for manipulating tests. We use *HeistT* with *HeistState*. In the "HelloWorld" template we need to evaluate two parameters, viz., *name* and *familyname*. Let's bind their values with state.

> bindValues :: HeistState IO -> HeistState IO
> bindValues s = let s1 = bindString "name" "Tom" s
>                    s2 = bindString "familyname" "Bombadil" s1
>                in s2


  <li> In the main function, load the templates to get the state, and then bind values to the tags *name* and *familyname*. Finally evaluate the template to produce the evaluated result which substitutes values for *name* and *familyname*.

> main :: IO ()
> main = do
>   Right st <- fmap bindValues <$> loadTemplateState
>   -- Eval the template "HelloWorld" in the context 
>   (Just template) <- evalHeistT (evalTemplate "HelloWorld") (X.Element "html" [] []) st
>   -- Get the evaluated template, and then render it on the console
>   let builder = X.renderHtmlFragment X.UTF8 template
>   hPutBuilder stdout builder

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-heist
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

This recipe looks at *heist* as a template framework where we loaded a template "HelloWorld" from a template directory. The template requires arguments *name* and *familyname*. The values of these arguments or tags are inserted in the *HeistState*. Then we run the *HeistT* monad with *evalHeistT* function by supplying the state.

The *heist* framework inserts the splices (named *HeistT*), and runs the evaluation for the template by using values supplied in the state.

