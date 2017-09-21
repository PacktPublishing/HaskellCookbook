Using Redis
===========

*Redis* (http://redis.io) is a key value store and more. It offers facilities much different than relational databases. As a NoSQL database, one has to employ different philosophy such as duplicating keys across stores, and maintaining reverse lookup etc. 

In this recipe, we will be using redis to create key value store, sorted sets, and hash sets. We will be using *hedis* library to connect to redis and manipulate the data.

Setting up...
-------------

<ul>
<li> Install *redis* from http://redis.io. On Microsoft Windows, use the windows port by Microsoft Open Tech Group at https://github.com/MicrosoftArchive/redis.
<li> Start *redis* in a default mode by simply running *redis-server* from the command line. You should see following messages on successful start. You might want to go with windows service on Microsoft Windows or a daemonized mode on unix flavoured systems. Optionally, you can also supply the configuration file.

~~~
d:\Tools\redis>redis-server.exe --maxheap 1G
                _._
           _.-``__ ''-._
      _.-``    `.  `_.  ''-._           Redis 2.8.2104 (00000000/0) 64 bit
  .-`` .-```.  ```\/    _.,_ ''-._
 (    '      ,       .-`  | `,    )     Running in stand alone mode
 |`-._`-...-` __...-.``-._|'` _.-'|     Port: 6379
 |    `-._   `._    /     _.-'    |     PID: 30944
  `-._    `-._  `-./  _.-'    _.-'
 |`-._`-._    `-.__.-'    _.-'_.-'|
 |    `-._`-._        _.-'_.-'    |           http://redis.io
  `-._    `-._`-.__.-'_.-'    _.-'
 |`-._`-._    `-.__.-'    _.-'_.-'|
 |    `-._`-._        _.-'_.-'    |
  `-._    `-._`-.__.-'_.-'    _.-'
      `-._    `-.__.-'    _.-'
          `-._        _.-'
              `-.__.-'

[30944] 01 Aug 09:21:07.275 # Server started, Redis version 2.8.2104
[30944] 01 Aug 09:21:07.275 * The server is now ready to accept connections on port 6379
~~~


<li> Usually, redis starts at port *6379*. 

  </ul>

How do we do it...
------------------
  <ul>

  <li> Create new project *using-hedis* with *simple* stack template.

~~~
  stack new using-hedis simple
~~~

  <li> Add dependency on *hedis* library in the *build-depends* sub-section of *executable* section. Also add the dependency on *bytestring* package. 

~~~
  executable using-hedis
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , hedis
                       , bytestring
~~~

  <li> Open *src/Main.hs*. We will be adding our source here.

  <li> Add initial module declaration and import required headers. Enable *OverloadedStrings* as we will be dealing with *ByteString* in this recipe.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Database.Redis
> import Data.ByteString.Char8 as B
> import Control.Monad
> import Control.Monad.IO.Class
> import Data.Maybe

  <li> Work with key-value store. *hedis* implements a composite data type called *RedisCtx m f*, where *m* is the context, and *f* is some sort of container. This way we can apply same code to both redis transactions (multiple commands sent atomically to redis), or outside transaction. For the commands sent outside transactions, the aboove type becomes *Redis (Either Reply a)* and when we apply it in the transactional scenario, it becomes *RedisTx (Queued a)*. Here we apply the commands outside transactions.

  Let's work out setting up keys and values. A key is a *ByteString* key. 

> createKV :: Redis ()
> createKV = do
>   -- Add exchange codes and their names
>   liftIO $ B.putStrLn "Setting stock exchange code and their descriptions"
>   set "XSES" "Singapore Stock Exchange"
>   set "XBSE" "Bombay Stock Exchange"
>   set "XNSE" "National Stock Exchange of India"
>   -- Delete a key
>   del ["XBSE"]
>   -- Get the values back
>   xses <- get "XSES"
>   xbse <- get "XBSE"
>   xnse <- get "XNSE"
>   -- Delete a key
>   let xchanges = (,,) <$> xses <*> xbse <*> xnse
>   liftIO $ print xchanges

  <li> Work with Lists. One can create a list simply by pushing values to it. A list can have duplicate values. It is possible to retreive the list by specifying the range and also delete the elements. 

> createList :: Redis ()
> createList = do
>   -- Push symbols in a list of stocks
>   liftIO $ B.putStrLn "Adding symbols to the stock list"
>   lpush "STOCKS" ["AAPL"]
>   lpush "STOCKS" ["GOOGL"]
>   lpush "STOCKS" ["FB"]
>   -- Get all symbols. (-1) indicates end of the range.
>   symbols <- lrange "STOCKS" 0 (-1)
>   liftIO $ print symbols
>   liftIO $ B.putStrLn "Changing some stocks and removing some"
>   -- Set a value to something else
>   lset "STOCKS" 0 "GOOGLE"
>   -- Remove all values for FB
>   lrem "STOCKS" 0 "FB"
>   symbols1 <- lrange "STOCKS" 0 3
>   liftIO $ B.putStrLn "Printing new stock list"
>   liftIO $ print symbols1

  <li> Work with hash set. In a hash set, one can create a hashset by a key. And set different fields of the hash set. 

> createHash :: Redis ()
> createHash = do
>   liftIO $ B.putStrLn "Set hashes for AAPL and FB"
>   hset "AAPL" "CATEGORY" "TECH"
>   hset "FB" "CATEGORY" "SOCIAL"
>   hmset "AAPL" [("HINT", "BUY"),("SENTIMENT","POSITIVE")]
>   -- Get FB Category
>   fbcat <- hget "FB" "CATEGORY"
>   liftIO $ B.putStrLn "Print FB Category"
>   liftIO $ print fbcat
>   -- Get multiple fields
>   aapls <- hmget "AAPL" ["HINT","SENTIMENT"]
>   liftIO $ B.putStrLn "What is suggestion for AAPL"
>   liftIO $ print aapls

  <li> Connect to the redis server and run the above functions

> main :: IO ()
> main = do
>   -- Connect with default information
>   conn <- checkedConnect defaultConnectInfo
>   runRedis conn $ do
>     createKV
>     createList
>     createHash
>   return ()


  <li> Build and execute the project.

~~~
  stack build
  stack exec -- using-hedis
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

*Hedis* wraps up the *redis* commands in a monad called *RedisCtx m f*. At first, this seems complicated, but it serves to run *redis* commands in either out of transactions or in a transactions. In the transactions, however, it is not possible to use the value of the command, as the commands are queued. The *hedis* library wraps *redis* commands with the functions of same name (but lowercase).

All the commands work with *ByteString*. Any serialization should be done to and from the *ByteString*.


