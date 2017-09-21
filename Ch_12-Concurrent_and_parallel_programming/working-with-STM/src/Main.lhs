Working with STM
================

In this recipe, we will work with *STM* (Software Transactional Memory) which provide atomic blocks for executions. They provide more guarantees about atomicity of the operation than *MVar*s. We will work with an example of a bank account, where simultaneous transactions are trying to do the transaction with the same bank account. 

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-STM* with *simple* stack template.

~~~
  stack new working-with-STM simple
~~~

  <li> Add *ghc-options* subsection to the *executable* section. Set the option to *-threaded*. Also add *stm* to the *build-depends* subsection.

~~~
  executable working-with-STM
    hs-source-dirs:      src
    main-is:             Main.hs
    ghc-options:         -threaded
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , stm
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Import *Control.Concurrent.STM* for importing *STM* module. 

> module Main where
>
> import Control.Concurrent.STM
> import Control.Concurrent
>

  <li> Define the account type which points to *TVar Int* which represents the current balance. 

> newtype Account = Account (TVar Int)

  <li> Define a function *transact* where the balance in the account can be modified. The transaction is not permitted if the the balance would become subzero. In such a case, the transaction is retried.

> transact :: Int -> Account -> STM Int
> transact x (Account ac) = do
>   balance <- readTVar ac
>   let balance' = balance + x
>   case balance' < 0 of
>     True -> retry
>     False -> writeTVar ac balance'
>   return balance'

  <li> Initialize bank account with some amount.

> openAccount :: Int -> STM Account
> openAccount i = do
>   balance <- newTVar i
>   return (Account balance)

  <li> Write a function to print the balance.

> printBalance :: Account -> IO ()
> printBalance (Account ac) = do
>   balance <- atomically (readTVar ac)
>   putStrLn $ "Current balance : " ++ show balance

  <li> Do the bank transaction in the *main* function. Open an *Account* with initial balance 100. Let's then try to debit 200 from a thread. Since we do not have that much balance, this should wait until there is a sufficient balance. From another thread, we do two credits of 75 each. After the sufficient balance has been made available, the debit should be allowed.

> main :: IO ()
> main = do
>   ac <- atomically $ openAccount 100
>   printBalance ac
>   forkIO $ do
>     balance <- atomically $ transact (-200) ac
>     putStrLn $ "Balance after debit : " ++ show balance
>
>   forkIO $ do
>     balance1 <- atomically $ transact 75 ac
>     putStrLn $ "Balance after credit of 75 : " ++ show balance1
>     balance2 <- atomically $ transact 75 ac
>     putStrLn $ "Balance after credit of 75 : " ++ show balance2
>
>   threadDelay (1000*1000) -- Wait for above actins to finish
>   printBalance ac
>   

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-STM
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

Unlike *MVar* where there is no control on subsequent options as far as atomicity is considerd. For example, reading the account balance and doing the transaction can be done separately with *MVar* and not together. *STM* on other hand, fully guarantee the atomicity of transaction in a *STM* monad.

In the above recipe, we use *TVar* (which are like *MVar* but used in the context of *STM*). We use *readTVar* and *writeTVar* for reading and writing *TVar*s. But all operations are enclosed in *STM* monad. We have to use *atomically* to run *STM* in the *IO* context.

In the *transact* function, we use *retry* in the context of *STM*. This indicates *STM* to retry the same action. In this case, it will result in blocked action till it becomes successful.


