
Static contents in Snap
=======================

In this recipe, we will serve the static contents, directories, files, images with Snap. 


How do we do it...
------------------
  <ul>

  <li> Create new project *static-contents-in-snap* with *simple* stack template.

~~~
  stack --resolver lts-9.1 new static-contents-in-snap simple
~~~

  <li> Add dependency on following libraries in the *build-depends* sub-section of *executable* section.

~~~
  executable static-contents-in-snap
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

  <li> Add a directory *static* in the project folder. Add *index.html* in this directory.

~~~
<!DOCTYPE HTML5>
<html>
  <body>
    <p> This file is served as a static content. You may add links to subfolder as well. But the folder ".." and absolute path are not honoured while serving the directory. </p>

    <p> This is a link to <a href="subfolder">subfolder</a> </p>
    
  </body>
</html>
~~~

  <li> Also add a subfolder named *subfolder*, and add following contents to the file *subfolder/example.html*.

~~~
<!DOCTYPE HTML5>
<html>
  <body>
    <p>
      This content is served from the folder <em>subfolder</em>
    </p>
  </body>
</html>
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add *Main* module and necessary imports.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Data.Monoid
> import Control.Applicative
> import Snap
> import Snap.Core
> import Snap.Http.Server
> import Snap.Util.FileServe

  <li> Serve the static contents using the function *serveDirectory*. The directory listing will be stylized by *fancyDirectoryConfig*. 

> main :: IO ()
> main = quickHttpServe $ serveDirectoryWith fancyDirectoryConfig "static"

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- static-contents-in-snap
~~~

  Point the browser to http://localhost:8000/subfolder. You should see following output.



  </ul>


How did we do it...
-------------------

In this recipe, the folder *static* is served by *quickHttpServe*. Note that in the absence of any *route*, the static folder serves the contents from the root path. The subfolders are automatically mapped to the subpath. *Snap* prevents access to parent folder of static directory, and also the absolute paths are not allowed. The function *defaultMimeTypes* in the module *Snap.Util.FileServe* gives a list of default mime types. It is possible to add own mime types in the list and serve the directory.

The function *fancyDirectoryConfig* uses own built-in style for listing directory. The *defaultDirectoryConfig* function shows the directory listing in a plain manner.

Note that the files *index.html*, *index.htm*, *default.html* etc. are automatically recognized as default indexes. 



