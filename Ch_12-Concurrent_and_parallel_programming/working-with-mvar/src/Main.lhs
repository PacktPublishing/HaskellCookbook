
Working with MVar
=================

In this recipe, we will look at *MVar* and *Chan* as basic ingredients to create concurent pipeline. We will create a Forex Order processing system, in which the orders to buy or sell currency are sent to an exchange. The exchange backend will process the orders, and print the summary.


How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-mvar* with *simple* stack template.

~~~
  stack new working-with-mvar simple
~~~

  <li> Add dependency on *containers* and *random* librares in the *build-depends* sub-section of *executable* section. Also add *-threaded* option to the *ghc-options* subsection.

~~~
  executable working-with-mvar
    hs-source-dirs:      src
    main-is:             Main.hs
    ghc-options:         -threaded
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , xxxx
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Define the main module, and import headers for using *MVar* and *Chan*.

> module Main where
> 
> import Control.Concurrent
> import Control.Concurrent.Chan
> import Data.Map
> import Control.Monad
> import System.Random

  <li> Represent an order as a *Currency* and associated amount represented as *Int*. We represent positive amount as **buy** and negative amount as **sell*. An *Exchange* is a channel which can accept orders. We need to stop the exchange at some point of time, hence, we represent *ProcessOrder* as a message sent to the *Exchange*. If *StopExchange* is sent to the exchange, the exchange would stop processing orders.

> type Currency = String
> type Amount = Int
> 
> data Order = Order Currency Amount deriving Show
> data ProcessOrder = ProcessOrder Order | StopExchange deriving Show
> type Exchange = Chan ProcessOrder

  <li> Define an backend for *Exchange* which will recceive the order, and maintains a register of *Currency* and *Amount* showing the current status. We represent current status by *MVar*. The *MVar* stores a *Map* of *Currency* and *Amount*.

> type Catalog = Map Currency Amount
> type Register = MVar Catalog

  <li> Add functions for managing catalog. We would like to add currency and amount to the catalog. Add *addOrder* to add an order to a catalog. The function *modifyOrder* will do the same thing as *addOrder*, but will return the tuple of modified *Catalog*. This will be used further in modifying *Register*.

> addOrder :: Order -> Catalog -> Catalog
> addOrder (Order c a) = insertWith (+) c a
>
> modifyOrder :: Monad m => Order -> Catalog -> m (Catalog, Catalog)
> modifyOrder order cat = let cat' = addOrder order cat
>                         in return (cat',cat')

  Add a function to print *Catalog* details.

> printCatalog :: Catalog -> IO ()
> printCatalog cat = forM_ (toAscList cat) $ \(c,a) -> do
>   putStrLn $ "Currency : " ++ c ++ ", Amount : " ++ show a

  <li> Once we receive an order, we would open the *Register* and add the order into it. Use *modifyOrder* for extracting *Catalog* from *Register* and modifying an order. The *processOrders* function, will continue to process orders and modify reigster till *StopExchange* message is received. 

> processOrders :: Exchange -> Register -> IO ()
> processOrders exch reg = do
>   po <- readChan exch
>   case po of
>     ProcessOrder o -> do
>       cat' <- modifyMVar reg (modifyOrder o)
>       putStrLn "Summary of orders"
>       printCatalog cat'
>       processOrders exch reg
>     StopExchange -> return ()
> 

  <li> Prepare for random data generation. Let consider three currencies, AUD - Australian Doller, SGD - Singapore Dollar and USD - US Dollar.

> currencies :: [Currency]
> currencies = ["AUD","SGD","USD"]

  Generate random sequence of currencies

> currenciesM :: Int -> IO [Currency]
> currenciesM i | i <= 0 = return []
> currenciesM i = do
>   c <- randomC
>   cs <- currenciesM (i-1) 
>   return (c : cs)
>     where
>       randomC = (currencies !!) <$> randomRIO (0,2) 

  <li> Similarly, generate a set of order amounts (either sell or buy).

> amounts :: Int -> IO [Amount]
> amounts i | i <= 0 = return []
> amounts i = do
>   sellOrBuy <- randomIO :: IO Bool
>   amount <- randomRIO (1,1000)
>   let orderAmount = if sellOrBuy then amount else (-1) * amount
>   orderAmounts <- amounts (i-1)
>   return (orderAmount:orderAmounts)

  Now get the set of random orders.

> orders :: Int -> IO [Order]
> orders i = zipWith Order <$> currenciesM i <*> amounts i

  Write a function to send orders every one second to the exchange.

> sendOrders :: [Order] -> Exchange -> IO ()
> sendOrders [] _ = return ()
> sendOrders (o:os) exch = do
>   putStrLn $ "Sending order " ++ show o
>   writeChan exch (ProcessOrder o)
>   threadDelay (1000*1000)
>   sendOrders os exch

  <li> In the *main* function, we create three order generators

> 
> main :: IO ()
> main = do
>   exch <- newChan :: IO Exchange
>   reg <- newMVar empty
>   -- Start the order processing backend
>   forkIO $ processOrders exch reg
>   -- Start order generators
>   forM_ [1..3] $ \_ -> forkIO $ do
>     os <- orders 10
>     sendOrders os exch
>
>   -- Wait for all orders to finish, and stop the exchange
>   threadDelay (1000*1000*15)
>   writeChan exch StopExchange
>
>   cat <- readMVar reg
>   putStrLn "Printing Final Summary of orders"
>   printCatalog cat

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-mvar
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

We use *MVar t* as a mutable location, that is either empty or contains a value of type *t*. We also use *Chan* which wraps up *MVar* as a stream. A *Chan* represents a FIFO stream. In this recipe, we use *Chan* to send orders simultaneously from multiple threads to a single channel *Exchange*. We then use backend processor *processOrder* which uses a *MVar* as for aggregating all these orders by aggregating them in a single *Map*. Following diagram, exemplifies the order processing system.



