
Managing Migrations
===================

When you are working on the databases, and trying to abstract the model on the backend at the same time, you already know that you will need a change in the model, or database or both at some point of time. Catering to changes in the requirements, performance criteria, the changes to database schema or the data model on the backend is inevitable. Changing the database schema without breaking the backend becomes one of the important, as well as time consuming task.

In this recipe, we will look at migrations and see how *persistent* approaches this issue. We will create a simple model and make a change to the model and run migrations again. We will be using *sqlite* backend. 


How do we do it...
------------------
  <ul>

  <li> Create new project *managing-migrations* with *simple* stack template.

~~~
  stack new managing-migrations simple
~~~

  <li> Add dependency on *persistent*, *persistent-template*, *persistent-sqlite*, *text*, and *mtl* libraries in the *build-depends* sub-section of *executable* section.

~~~
executable manage-migrations
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , persistent
                     , persistent-template
                     , persistent-sqlite
                     , text
                     , mtl
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

  <li> Create the model for the database. The model below represents a *marine vessel* which has structures and compartments arranged hierarchically. 

> share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
> Asset
>    name Text
> Structure
>    name Text
>    parent StructureId Maybe
>    deriving Show
> Compartment
>    name Text
>    parent CompartmentId Maybe
> |]

  <li> The function *mkMigrate "migrateAll"* creates a function *migrateAll* which inspects the existing database for the tables, and emits only SQL statements required to change existing schema to achieve intended schema.

  <li> Write *main* function to run the migration. Create a database file "ship.db" and run the migration against the database.

> main :: IO ()
> main = runSqlite "ship.db" $ runMigration migrateAll

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- managing-migrations
~~~

  You should see following output,


  <li> However, we realise that we have to maintain multiple assets in the database, and a structures and compartments always belong to one and only one asset at a time. Hence we have to add a reference to the asset in the *Structure* as well as *Compartment*. At the same time, it is not necessary to have a name for a structure. Sometimes, internal structures in a ship are just given a number (primary key, in our case), and not a name. Hence we make name of a structure optional. Make the changes in the definition of the model. Change the above model to

~~~
 share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
 Asset
    name Text
 Structure
    name Text Maybe
    parent StructureId Maybe
    owner AssetId
    deriving Show
 Compartment
    name Text
    owner AssetId
    parent CompartmentId Maybe
 |]
~~~

  <li> Now again run the main, but this time, instead of calling *runMigration*, call *printMigration*. You should see following output after building and executing. 

~~~
CREATE TEMP TABLE "structure_backup"("id" INTEGER PRIMARY KEY,"name" VARCHAR NULL,"parent" INTEGER NULL REFERENCES "structure","owner" INTEGER NOT NULL REFERENCES "asset");
INSERT INTO "structure_backup"("id","name","parent") SELECT "id","name","parent" FROM "structure";
DROP TABLE "structure";
CREATE TABLE "structure"("id" INTEGER PRIMARY KEY,"name" VARCHAR NULL,"parent" INTEGER NULL REFERENCES "structure","owner" INTEGER NOT NULL REFERENCES "asset");
INSERT INTO "structure" SELECT "id","name","parent","owner" FROM "structure_backup";
DROP TABLE "structure_backup";
CREATE TEMP TABLE "compartment_backup"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"owner" INTEGER NOT NULL REFERENCES "asset","parent" INTEGER NULL REFERENCES "compartment");
INSERT INTO "compartment_backup"("id","name","parent") SELECT "id","name","parent" FROM "compartment";
DROP TABLE "compartment";
CREATE TABLE "compartment"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"owner" INTEGER NOT NULL REFERENCES "asset","parent" INTEGER NULL REFERENCES "compartment");
INSERT INTO "compartment" SELECT "id","name","owner","parent" FROM "compartment_backup";
DROP TABLE "compartment_backup";
~~~

  You can see that the migration has taken care to alter the schema by adding references to *Asset*. At the same time, it also creates the SQL statements to copy the data from old tables to modified.

  </ul>


How did we do it...
-------------------

In this recipe, we have run the migration against an empty database, where migration created the schema from the scratch. When we modified the model, and ran the migration against existing database *ship.db*, the migration detected the change, and created the migration script. It is a good idea to *print* the migration than *running* the migration. It would give a chance to rectify any errors in the migration.

In fact, in the example that we have seen, we have added an extra reference to *Asset* as a foreign key in *Structure* and *Compartment*. This would create a problem during migration, as we will not have reference to an asset in the old data. 
