Working with Persistent
=======================

To be able to write a backend, or a storage service, it is imperative that we will at some time think about storing the data in a relational database, or a binary serializable format or a file such as JSON or YAML. In this recipe, we will work with *persistent* library, which is designed to abstract the concept of defining the schema (data model and relations among them), and working with a storage backend (such as *sqlite*, *postgresql* etc.).

In this recipe, we will create a model to store following data -

* User details (*User Name, Email*)
* Stock that the user is interested in (*Exchange, Symbol*)

We will use *sqlite* as a backend, as it does not require any installation. But the model defined here can be worked out with any *persistent* backend such as *postgresql* etc. The model definition in *peristent* library is created through *Template Haskell*. A template haskell enables programmers to generate code at the compile time. This involves writing macros, generating code etc. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-persistent* with *simple* stack template.

~~~
  stack new working-with-persistent simple
~~~

  <li> Add dependency on two libraries, viz., *persistent*, and *persistent-sqlite*, in the *build-depends* sub-section of *executable* section. Also add *persistent-template*, *text*, and *mtl* (monad transformers) in the same section.

~~~
executable working-with-persistent
  hs-source-dirs:      src
  main-is:             Main.lhs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , persistent
                     , persistent-sqlite
                     , persistent-template
                     , text
                     , mtl
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> At the top, we need to enable many extensions that are required for *persistent* backend.

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

  These are some features which can be enabled by putting them in the pragmas as it is done here. We have already seen *OverloadedStrings* in the last chapter. These features enable specific extension in GHC. We will be looking at few extensions in next chapters. The extension that is more relevant for this recipe is *TemplateHaskell*.

  <li> Write the module declaration for *Main* module

> module Main where

  <li> Import the modules that we will need for defining the entities and relations between them. Note the modules *TH* usually indicate that template haskell is required to use it. 

> import Database.Persist.TH
> import Data.Text as T
> import Database.Persist.Sqlite
> import Database.Persist.Sql as S
> import Control.Monad.Reader
> import Control.Monad.IO.Class

  <li> Define the model. We will define three tables, *Stock*, *User* and *UserStock*. The *Stock* stores information about stock, exchange, symbol, whereas *User* stores the information about user's name and email id. We will declare that the email should be unique. *UserStock* is an association between user and stock. 

> share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
> Stock
>   exchange Text
>   symbol Text
>   UniqueStockId exchange symbol
>   deriving Show
> User
>   name Text
>   email Text
>   UniqueEmailId email
>   deriving Show
> UserStock
>   userid UserId
>   stockid StockId
>   Primary userid stockid
>   deriving Show
> |]
> 

  Note the *Template Haskell* syntax, where we enclose the expressions in square brackets, define tables by simply indenting, and specifying the type of the data. Also note how we have created the foreign key reference to *User* and *Stock* in the table *UserStock*, where a composite key is defined with fields *(userid, stockid)*. Here *userid*, and *stockid* refer to *uid* in *User* table, and *sid* in *Stock* table respectively.

  <li> The above *template haskell* code will result in following data types *User*, *Stock* and *UserStock*.

  <li> Create the tables and schema. The database operations, run in a monad *m*, which supports *IO* as well (as denoted by *MonadIO* type class). Note that we write the query generically, and are oblivious as to which backend database will be used. 

> createSchema :: (Monad m, MonadIO m) => ReaderT SqlBackend m ()
> createSchema = runMigration migrateAll

  <li> Insert few users, and stocks. Associate users and stocks.

> insertData :: (Monad m, MonadIO m) => ReaderT SqlBackend m (Key User, Key User, Key Stock, Key Stock)
> insertData = do
>   johnid <- insert $ User "John Broker" "john@example.com"
>   liftIO $ putStrLn $ "Added user John" ++ show johnid 
>   janeid <- insert $ User "Jane Investor" "jane@example.com"
>   liftIO $ putStrLn $ "Added user Jane" ++ show janeid
>
>   -- Insert few stocks
>   dbsid <- insert $ Stock "XSES" "D05"
>   liftIO $ putStrLn $ "Added Singapore Exchange DBS stock" ++ show dbsid
>   infyid <- insert $ Stock "XNSE" "INFY"
>   liftIO $ putStrLn $ "Added NSE India, Infosys stock" ++ show infyid
>
>   -- Associate the user with stock
>   john_d05 <- insert $ UserStock johnid dbsid
>   liftIO $ putStrLn $ "John subscribed to DBS stock" ++ show john_d05
>   john_infy <- insert $ UserStock johnid infyid
>   liftIO $ putStrLn $ "John subscribed to INFY stock" ++ show john_infy
>   jane_d05 <- insert $ UserStock janeid dbsid
>   liftIO $ putStrLn $ "Jane subscribed to DBS stock" ++ show jane_d05
>   return (johnid, janeid, dbsid, infyid)

  <li> Do query to get stocks associated with the user

> queryUserStockCount :: MonadIO m => Key User -> ReaderT SqlBackend m Int
> queryUserStockCount user = do
>   S.count [UserStockUserid ==. user]

  <li> Delete a stock from the user

> deleteUserStock :: MonadIO m => UserId -> StockId -> ReaderT SqlBackend m ()
> deleteUserStock user stock = do
>   S.delete (UserStockKey user stock)

  <li> Update a name of a user

> updateUserName :: MonadIO m => UserId -> Text -> ReaderT SqlBackend m ()
> updateUserName user newname =
>   S.update user [UserName =. newname]

  <li> Use the above functions to create the schema, add data to it, and manipulate it. We are running against in memory *Sqlite* database. 

> main :: IO ()
> main = runSqlite ":memory:" $ do
>   createSchema
>   (johnid, janeid, dbsid, infyid) <- insertData
>   count <- queryUserStockCount johnid
>   liftIO $ putStrLn $ "John has " ++ show count ++ " stocks"
>   liftIO $ putStrLn $ "Delete John's DBS stock"
>   deleteUserStock johnid dbsid
>   count1 <- queryUserStockCount johnid
>   liftIO $ putStrLn $ "Now John has " ++ show count1 ++ " stocks"
>   liftIO $ putStrLn $ "Change Jane's name"
>   updateUserName janeid "Jane Quant"
>   -- Retrieve new name
>   jane <- get janeid
>   liftIO $ putStrLn $ "Jane's name is now " ++ show jane
>   return ()

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-persistent
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In the above recipe, we have touched upon many aspects of defining, storing and querying the model. Let us look at each aspect one by one.

* **Defining the Model**

  The definition of the model starts with following statement -
~~~
  share [mkPersist sqlSettings, mkMigrate "migrateAll"]
~~~

  The above definition tells that how we would like to persist the model. *sqlSettings* denotes that we would like to use SQL backend for storign the model. *mkMigrate* takes a string argument. This should be name of the function (in this case *migrateAll*) that represents creation of all the schema defined in the model.

  The model itself is defined within *[persistLowerCase| ...]*. It tells to persist the name of tables and fields using lower case letters. The following definition of *Stock* is converted into two representations - Haskell Data Type, and its corresponding SQL representation.

~~~
 Stock
   exchange Text
   symbol Text
   UniqueStockId exchange symbol
   deriving Show
~~~

  At the command prompt, do following

~~~
  stack ghci
~~~

  It should open up GHCi interactive prompt, inspect the types of *User*, *Stock*, and *UserStock* (by using command such as *:i User*).  Note that the generated data types are defined using GADTs (Generic Algebraic Data Types). In the model definition, We use following convention, the first unindented line specifies the name of the table (or data type). The next indented lines define the fields. For example *exchange Text* defines a field *exchange* with type *Text*. The *persistent* maps haskell data types to compatible data type in the backend such as *SQLite*, *Postgresql* etc. 

  It is also possible to specify the constraints. In the definition of *Stock* table, we have created unique constraints for two fields together viz., *exchange*, and *symbol*. For the table *UserStock*, the primary key is a composite key comprising of *userid* and *stockid*. In fact both *userid* and *stockid* are foreing keys for the tables *User* and *Stock* respectively.

~~~
  Stock
~~~

~~~
data User = User {userName :: !Text, userEmail :: !Text}
data Stock = Stock {stockExchange :: !Text, stockSymbol :: !Text}
data UserStock
  = UserStock {userStockUserid :: !Key User,
               userStockStockid :: !Key Stock}
~~~

Also note that for the *User* and *Stock*, we did not specify the key, the key type is generated automatically by *persistent* as -

~~~
newtype instance Key User = UserKey {...}
newtype instance Key Stock = StockKey {...}
data instance Key UserStock = UserStockKey {...}
~~~

You will notice that the *persistent* also defines the Key type for the user as *UserId*. This type is a synonym of *Key User*. In the definition of *userid* field of *UserStock*, we specify *UserId* as the type. This way the foreign key constraint is automatically created. It is also possible to create foreign key constraint by specifying *Foreign* in the model definition.

If the primary key is not given, then *persistent* creates default integer based, auto-incremented key.

For every field, a type *TableField* is created. For example, for the field *name* of *User*, a type *UserName* is created. This is used in the expressions for *query*, *update* and *delete* functions. By using a specific type, we are indicating a specific field in a data structure. This way, we can do type safe queries over the database.

At the command prompt, in GHCi, you can also run *runSqlite* with *user.db* as an argument.

~~~
*Main> runSqlite "user.db" createSchema
~~~

This will create sqlite database *user.db* in the current directory, and create the schema. You can open ths database file using *sqlite* executable. You can print the schema, by using command *.schema*. It should print following -

~~~
CREATE TABLE IF NOT EXISTS "stock"("id" INTEGER PRIMARY KEY,"exchange" VARCHAR NOT NULL,"symbol" VARCHAR NOT NULL,CONSTRAINT "unique_stock_id" UNIQUE ("exchange","symbol"));
CREATE TABLE IF NOT EXISTS "user"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"email" VARCHAR NOT NULL,CONSTRAINT "unique_email_id" UNIQUE ("email"));
CREATE TABLE IF NOT EXISTS "user_stock"("userid" INTEGER NOT NULL REFERENCES "user","stockid" INTEGER NOT NULL REFERENCES "stock", PRIMARY KEY ("userid","stoc
d"));
~~~

You can also run the *main* with this file, and later inspect the data created.




