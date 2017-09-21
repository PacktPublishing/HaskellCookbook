Using Hashset and Sorted Sets in Redis to create Trie
=====================================================

In this recipe, we will be using *hedis* to create simple *trie* in *redis*. We will use *hashset* to store an object, and store its searchable *index* in the *sorted set* in redis. We will be using prefix trie to create a searchable index. For example, if we are searching for "APPLE", we will index all prefixes ("A","AP","APP","APPL", and "APPLE") in the index. Whenever a user is entering a string to search, we will be able to look up our index and get the result back.

Start the redis server, and note down connection info. In this recipe, we will assume that redis is working on the same machine at the port 6379. This is what *hedis* assumes to connect to *redis* server using default connection information.


How do we do it...
------------------
  <ul>

  <li> Create new project *hedis-trie* with *simple* stack template.

~~~
  stack new hedis-trie simple
~~~

  <li> Add dependency on *hedis* library in the *build-depends* sub-section of *executable* section. 

~~~
  executable hedis-trie
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
> import Prelude as P
> import Database.Redis
> import Data.ByteString.Char8 as B
> import Data.Char
> import Data.Monoid
> import Control.Monad
> import Control.Monad.IO.Class

  <li> Write a function to take a sentence, break it into words, and return prefixes of all the words.

> prefixes :: B.ByteString -> [[B.ByteString]]
> prefixes = P.map (P.tail . B.inits) . B.words

  <li> Write a function to take the prefixes, a key to hash and add to a sorted set. It also returns number of keys updated. Since we are working with list of lists, we use two *foldM*. In the sorted set, we associate each hash key exactly one with score 0.0. 

> addKeys :: (RedisCtx m f, Applicative f) => [[B.ByteString]] -> B.ByteString -> m (f Integer)
> addKeys prefixes hashkey = 
>    let addtrie i p = do
>          rs <- zadd p [(0.0, hashkey)]
>          pure $ (+) <$> i <*> rs
>        addtries ps = foldM addtrie (pure 0) ps
>        addtriesS s ps = do
>          rs <- addtries ps
>          pure $ (+) <$> s <*> rs
>    in foldM addtriesS (pure 0) prefixes

  <li> Create hash for a stock symbol, and its name

> addSymbol :: (RedisCtx m f, Applicative f) => B.ByteString -> B.ByteString -> m (f Bool)
> addSymbol symbol name = do
>   hset symbol "NAME" name

  <li> Prepare some data to be added to *redis*. We will add all symbols from *Singapore* exchange. The symbols and its name is embedded in the code. 

> stockData :: [(B.ByteString, B.ByteString)]
> stockData = [ ("MT1", "Dragon Group International Ltd")
>             , ("BKV",	"Dukang Distillers Holdings Ltd")
>             ,("CZ4", "Dutech Holdings Ltd")
>             ,("5SO", "Duty Free International Ltd")
>             ,("NO4", "Dyna-Mac Holdings Ltd")
>             ,("D6U", "Dynamic Colours Ltd")
>             ,("BDG", "Eastern Holdings Ltd")
>             ,("BWCU", "EC World Real Estate Investment Trust")
>             ,("5CT", "EcoWise Holdings Ltd")
>             ,("5HG", "Edition Ltd")
>             ,("42Z", "Eindec Corporation Ltd")
>             ,("E16", "Elec & Eltek International Co Ltd")
>             ,("BIX", "Ellipsiz Ltd")]

  <li> Take the above data and add it to the *redis* server. The symbol, and its name is added to the hash set, whereas all the prefixes for the name (by separating into words) are added to sorted sets. Each prefix will create a new *sorted set*.

> addData :: (RedisCtx m f, Applicative f) => [(B.ByteString, B.ByteString)] -> m ()
> addData stocks = do
>   forM_ stocks $ \(stock, name) -> do
>     addSymbol stock name
>     -- convert name into lower case so that we can do a generic search
>     let nameL = B.map toLower name
>         namePs  = prefixes nameL
>     addKeys namePs stock

  <li> Search the stock, and return the list of stocks

> searchStocks :: B.ByteString -> Redis [B.ByteString]
> searchStocks search = do
>   stocks <- zrange search 0 (-1)
>   case stocks of
>     Right ss -> forM ss $ \s -> do
>       n <- hget s "NAME"
>       case n of
>         Right (Just name) -> return $ s <> ": " <> name
>         _                 -> return $ s <> ": name not found ***error***"
>   

> main :: IO ()
> main = do
>   conn <- checkedConnect defaultConnectInfo
>   runRedis conn $ do
>     liftIO $ B.putStrLn "Adding stocks to the redis trie index"
>     addData stockData
>     liftIO $ B.putStrLn "Seaching for strings"
>     found1 <- searchStocks "holdi"
>     liftIO $ do
>       B.putStrLn "Results for \"holdi\""
>       forM_ found1 B.putStrLn
>     found2 <- searchStocks "dyna"
>     liftIO $ do
>       B.putStrLn "Results for \"dyna\""
>       forM_ found2 B.putStrLn

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- hedis-trie
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In this recipe, we used redis *sorted set* for storing the search data. A *sorted set* stores the data sorted with a *score*. Each score should be attached with only a single value. Here, we get the name of the stock, such as "EcoWise Holdings Ltd", and "Eastern Holdings Ltd". We convert them into prefixes, as discussed above. Since both the names contains word "Holdings", we will have a sorted set with a key "holdi" (remember that we convert all names to lowercase), and two values -

~~~
holdi - score = 0.0, value = 5CT
holid - score = 0.0, value = BDT
~~~

When we search for the string "hold", the both the values should be returned. From the values, we find the name from the hash set, and return the set of names. We can further optimize the search by increasing the score for commonly searched stocks. Further, if multiple strings are searched, then we can also take intersection of two sorted sets, and create a temporary sorted set (with *TTL* or specifying time to live) and search from the new temporary set. 


