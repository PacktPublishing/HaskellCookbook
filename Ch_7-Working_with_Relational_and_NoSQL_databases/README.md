In this chapter, we will work with following recipes,

* Using Persistent to define, insert, query, update and delete the model
* Using Persistent to manage migrations
* Using Persistent to define custom datatype
* Using Esquellete to do advanced queries
* Use Hedis as a key-value serialization
* Use sorted sets for Sorting collection 
* Use Hashset for storing objects
* Use Hashset alongwith Sorted sets for creating indexed queries

Introduction
===============

Till the earlier chapter, we have looked at Haskell language features, type classes, collections and worked with various examples. But all those constructs were purely Haskell features. In this chapter, we will be interfacing with outside world (apart from console), by interacting with databases.

We will use *persistent* library to work with relational databases. Using the *persistent* model, we will define the relations, do a query, insert, update and deletion on the stored data. We will move on to *esquellete* to advanced queries. The *esquellete* defines a DSL (*Domain Specific Language*) so that we can do advanced queries.

We will then move to *hedis* a backend for *redis* database. We will work with *hedis* query, and work our way towards key-value pairs, sorted sets, and hashsets.

The *persistent* library makes use of *Template Haskell* to create the relational model. We will be using *sqlite3* as our backend for storing data, but connecting with other databases such as *postgresql* and *mysql* should be similar, as *persistent* provides a database agonostic way of defining the entities, relations and queries. 


