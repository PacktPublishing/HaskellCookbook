Log Analysis with Map
=====================

In this recipe, we will be using map to analyse the access log for apache web server. The log contains access parameters for each host accessing the web server per line. The log looks like following -

~~~
64.242.88.10 - - [07/Mar/2004:16:10:02 -0800] "GET /mailman/listinfo/hsdivision HTTP/1.1" 200 6291
64.242.88.10 - - [07/Mar/2004:16:11:58 -0800] "GET /twiki/bin/view/TWiki/WikiSyntax HTTP/1.1" 200 7352
64.242.88.10 - - [07/Mar/2004:16:20:55 -0800] "GET /twiki/bin/view/Main/DCCAndPostFix HTTP/1.1" 200 5253
~~~

The line starts with _host name_ or _ip_ which is accessing the web server. The remaining part of the line includes date and time, method of access (__GET__,__PUT__,__POST__ etc.) and path on the web server being accessed. The server also prints status information.

How do we do it...
------------------

  <ul>
  <li> Create a new project *log-parser* with *simple* stack template.

~~~
  stack new log-parser simple
~~~

  <li> Open *log-parser.cabal* and add the library *containers* as a dependent library, in the sub-section *build-depends* of section *executable*.

~~~
executable log-parser
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , containers
~~~

> module Main where

  <li> Import modules for *file IO* and *strict map*

> import System.IO
> import qualified Data.Map.Strict as M
> import System.Environment
> import Control.Monad

  <li> Read the the file line by line.

> hLines :: Handle -> IO [String]
> hLines h = do
>  isEOF <- hIsEOF h
>  if isEOF then
>    return []
>  else
>    (:) <$> hGetLine h <*> hLines h

  <li> We are only interested in the host or ip. Grab it. Return an empty string if we are presented with empty list.

> host :: [String] -> String
> host (h:_) = h
> host _     = ""

  <li> Convert the list of lines into list of host names by using functions *words* (to convert a line into words) and *host* (to take only first of those words).

> hosts :: Handle -> IO [String]
> hosts h = fmap (host . words) <$> hLines h

  <li> Given a hostname and and a map, add the hostname to the map with access count 1. If the host is already present in the map, then add the counts.

> updateAccessCount :: String -> M.Map String Int -> M.Map String Int
> updateAccessCount h mp = M.insertWith (+) h 1 mp

  <li> Fold over the list of hosts, starting with an empty map, and adding the hostname with the access count. Use the function updateAccessCount to combine the host name (or ip) with the access count map.

> foldHosts :: [String] -> M.Map String Int
> foldHosts = foldr updateAccessCount M.empty

  <li>Get the data from _http://www.monitorware.com/en/logsamples/apache.php_. The data is free to be used. We give the relative path of the log file as an argument. Then proceed to get a map. We then convert the map to the list, and then print the names of the host and their access count.

> main :: IO ()
> main = do
>  (log:_) <- getArgs
>  accessMap <- withFile log ReadMode (fmap foldHosts . hosts)
>  let accesses = M.toAscList accessMap
>  forM_ accesses $ \(host, count) -> do
>    putStrLn $ host ++ "\t" ++ show count

  <li>Build and run the project

~~~
  stack build
  stack exec -- log-parser access_log/access_log
~~~

  The output should print the statistics of host name or ip against its accesses.

  </ul>

How did we do it...
-------------------
  * The recipe reads the file line by line. Each line is split into words by using function *words*. Since we are interested in only the host or its ip, we take only first word. 
  * Each host name / ip is considered as a tuple (host name, 1) where 1 denotes that each entry corresponds to single access.
  * We use *insertWith* function to insert the above entry. If the entry already exists in the map, then we use combining function to add 1 to the existing access count.
  * We get the hosts in the ascending order using function *toAscList*. We use *Control.Monad.forM_* to iterate over the list of accesses, and print them to the console.


