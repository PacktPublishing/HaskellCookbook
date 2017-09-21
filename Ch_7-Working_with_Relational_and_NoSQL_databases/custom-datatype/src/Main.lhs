
Creating Custom Data Type
=========================

In the model definition for *persistent*, we can use data type such as *Int*, *Text*, *Int64* etc. They are translated to proper SQL data types according to the SQL dialect that we are working with. Sometimes, the supported data types are not sufficient for our needs, and we might want to write our custom data type.

In this recipe, we will write a custom 


How do we do it...
------------------
  <ul>

  <li> Create new project *custom-datatype* with *simple* stack template.

~~~
  stack new custom-datatype simple
~~~

  <li> Add dependency on *persistent*, *persistent-template*, *persistent-sqlite*, *text*, and *mtl* libraries in the *build-depends* sub-section of *executable* section. Also add *email-validate* as a dependency. We will use it to store grammatically valid email addresses. Also add a module *Custom* to *other-modules* subsection in the same section. (You will have to add this subsection). The *other-modules* represents a set of modules that are part of compilation but not exposed to user. We will be adding *Custom* module for defining custom data type.

~~~
  executable custom-datatype
    hs-source-dirs:      src
    main-is:             Main.hs
    other-modules:       Custom
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , persistent
                       , persistent-template
                       , persistent-sqlite
                       , text
                       , mtl
                       , email-validate
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> Add the extensions required for invoking *persistent* and *persistent-template* functions.

> {-# LANGUAGE EmptyDataDecls             #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GADTs                      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE QuasiQuotes                #-}
> {-# LANGUAGE TemplateHaskell            #-}
> {-# LANGUAGE TypeFamilies               #-}
> {-# LANGUAGE DeriveGeneric              #-}

  <li> Add the declaration for the *Main* module.

> module Main where

  <li> Add the required imports.

> import Database.Persist.TH
> import Data.Text as T
> import Database.Persist.Sqlite
> import Database.Persist.Sql as S
> import Control.Monad.Reader
> import Control.Monad.IO.Class
> import Text.Email.Validate
> import Custom

  <li> Open a new file in the same directory called *Custom.hs*. Add customary extension, module declaration, etc to the file. Note that the custom file must be defined in a separate module, else the module definition produces an error. 

~~~
{-# LANGUAGE TemplateHaskell #-}
module Custom where

import Database.Persist.TH
import Text.Email.Validate
~~~

  <li> Add the user status, *Active* and *Inactive*. Use *derivePersistField* to create a custom data type. 

~~~
  data Status = Active | Inactive
              deriving (Show, Eq, Read)

  derivePersistField "Status"
~~~

  <li> Define the custom field for email address.

~~~
  derivePersistField "EmailAddress"
~~~

  <li> Close the file, and return to *src/Main.hs*. Write the model definition. 

> share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
> User
>   status Status
>   email EmailAddress
> |]

  <li> Add the data, and query it..

> sampleData :: MonadIO m => ReaderT SqlBackend m ()
> sampleData = do
>   let Right jupitermail = validate "jupyter@planets.com"
>       Right plutomail = validate "pluto@planets.com"
>       Right earthmail = validate "earth@planets.com"
>   insert $ User Custom.Active jupitermail
>   insert $ User Custom.Active earthmail
>   insert $ User Custom.Inactive plutomail
>   return ()
> 

> main :: IO ()
> main = runSqlite ":memory:" $ do
>   runMigration migrateAll
>   sampleData
>   -- Get all users
>   all <- S.count ([] :: [Filter User])
>   active <- S.count ([UserStatus ==. Custom.Active])
>   liftIO $ putStrLn $ "There are " ++ show all ++ " users"
>   liftIO $ putStrLn $ show active ++ " are active"

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- custom-datatype
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

Creating a custom data type using *persistent* is easy. You can make use of *template haskell* to manufacture the custom data type. The template haskell take advantage of the *Show* and *Read* instance of the data to convert the data to and from the *String* representation.

