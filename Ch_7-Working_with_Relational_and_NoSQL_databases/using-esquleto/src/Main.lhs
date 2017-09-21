Using Esqueleto
===============

We have used *persistent* library and SQL expressions using *Database.Persist.SQL* module. We have used the generated types for each field in the filter, insert and update expressions. But the complexity of the query can increase rapidly. Of course, there is a way to do a plain SQL query with *persistent* library. Here, in this recipe, we will be using *esqueleto* library to do complex query such as joins etc.

In this recipe, we will write complex SQL query, which is type safe, as well as easy to write as well.  Being *type safe* is good, because we will catch if there is any major issue earlier on! 

How do we do it...
------------------
  <ul>

  <li> Create new project *using-esqueleto* with *simple* stack template.

~~~
  stack new using-esqueleto simple
~~~

  <li> Add dependency on *persistent*, *persistent-template*, *persistent-sqlite*, *text*, and *mtl* libraries in the *build-depends* sub-section of *executable* section. Also add *esqueleto* to the same sub-section.

~~~
  executable using-esqueleto
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , persistent
                       , persistent-template
                       , persistent-sqlite
                       , text
                       , mtl
                       , esqueleto
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
> import Data.Text as T hiding (count, groupBy)
> import Database.Persist.Sqlite (runSqlite)
> import Control.Monad.Reader
> import Control.Monad.IO.Class
> import Database.Esqueleto

  <li> Create the model for the database. The model below represents a referral system in which one user can refer other users. This is usually used to award a user who can help pull in more number of users. 

> share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase| 
> User
>     email           Text
>     UniqueEmail     email
>     referredBy      UserId Maybe
>     verified        Bool
>     deriving Show
> |]

  The above model represents user as having a unique email address. The user can register herself, or she can be referred by other users. Mererly registration does not help, the user also has to validate his/her address (usually by clicking on the link sent for verification). 

  <li> Write a query to get users with referral greater than 0. Note that only verified users count.

> getAllRefCounts :: MonadIO m => SqlPersistT m [(Value Text, Value Int)]
> getAllRefCounts = 
>     select $ from $ \(p `InnerJoin` r) -> do
>       on (r ^. UserReferredBy ==. just (p ^. UserId))
>       where_ (r ^. UserVerified ==. val True)
>       groupBy (p ^. UserEmail, p ^. UserId)
>       let cr = count (r ^. UserId )
>       orderBy [ desc cr ]
>       return (p ^. UserEmail, cr)


  <li> Add data to the referral system. Add users referred by others. One user hasn't verified his email yet.

> createData :: MonadIO m => SqlPersistT m ()
> createData = do
>   a <- insert $ User "a@example.com" Nothing True
>   b <- insert $ User "b@example.com" (Just a) True
>   insert $ User "c@example.com" (Just a) True
>   insert $ User "d@example.com" (Just b) True
>   insert $ User "e@example.com" Nothing True
>   insert $ User "f@example.com" (Just a) False
>   return ()


> main :: IO ()
> main = runSqlite ":memory:" $ do
>   runMigration migrateAll
>   createData
>   referrals <- getAllRefCounts
>   liftIO $ putStrLn "Referral counts"
>   liftIO $ print referrals

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- using-esqueleto
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In this recipe, we did a self join to get the referral count for the users. The query using *esqueleto* is given below.

~~~
     select $ from $ \(p `InnerJoin` r) -> do
       on (r ^. UserReferredBy ==. just (p ^. UserId))
       where_ (r ^. UserVerified ==. val True)
       groupBy (p ^. UserEmail, p ^. UserId)
       let cr = count (r ^. UserId )
       orderBy [ desc cr ]
       return (p ^. UserEmail, cr)
~~~

The query looks very similar to SQL itself. The *esqueleto* defines monadic DSL for writing queries in the tune of SQL. For example a query "select * from users" will become following in *esqueleto*.

~~~
  select (from $ \user -> return user)
~~~

If we are searching for a particular user, then we can write,

~~~haskell
  select (from $ \user -> do
    where_ (user ^. UserEmail ==. val "a@example.com")
    return user
  )
~~~

In our example, we have used *InnerJoin* on two tables. We have specified this by *p `InnerJoin` q*. We then added the criteria using *on*, *where_*, *groupBy*, *orderBy* and *count* functions, which translate to corresponding SQL keywords (*ON, GROUPBY, ORDERBY and COUNT*)
