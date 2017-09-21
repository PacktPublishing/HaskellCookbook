
Working with Cloud Haskell
==========================

So far we have seen multi-threaded concurrency. In this recipe, we will look at distributed concurrency, where the concurrency can be achieved through multiple processes either on the same machine or a cluster of machines. In this recipe, we will create a *local* node and communicate with it using *tcp* transport. We will use *cloud-haskell* libraries for doing this.

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-cloud-haskell* with *simple* stack template.

~~~
  stack new working-with-cloud-haskell simple
~~~

  <li> Add dependency on *distributed-process* library in the *build-depends* sub-section of *executable* section. Also add support libraries as shown below. Also add *-threaded* option to *ghc-options*.

~~~
  executable working-with-cloud-haskell
    hs-source-dirs:      src
    main-is:             Main.hs
    ghc-options:         -threaded
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , binary
                       , distributed-process
                       , network-transport
                       , network-transport-tcp
~~~

  At the moment of writing this recipe, the cloud haskell libraries are not part of stack *LTS*. Hence we need to add more dependency in *stack.yaml* in the *extra-deps* section. Note that the version numbers may change in future, and you might have to adjust them.

~~~
extra-deps:
  - distributed-process-0.6.6
  - syb-0.6
  - network-transport-tcp-0.5.1
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

> {-# LANGUAGE DeriveGeneric #-}
> module Main where
>
> import Control.Concurrent
> import Control.Monad
> import Control.Distributed.Process
> import Control.Distributed.Process.Node
> import Network.Transport.TCP
> import Network.Transport (Transport)
> import Data.Binary (Binary)
> import GHC.Generics

  <li> Create a transport serving at port 10501.

> localTransport :: IO Transport
> localTransport = do
>   Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
>   return t
>

  <li> Create a *Process* to welcome the user with a greeting.

> type UserId = String
>
> data UserIntimation = UserIntimation ProcessId UserId deriving (Show, Generic)
>
> instance Binary UserIntimation 
> 
> welcome :: UserIntimation -> Process ()
> welcome (UserIntimation pid uid) = send pid $ "Welcome to Cloud Haskell, " ++ uid
> 

  <li> Create a *Process* to define some distributed work. Accept some message and give back some reply.

> greet :: Process ()
> greet = forever $ receiveWait [match welcome]


  <li> Run the nodes in the *main* function

> main :: IO ()
> main = do
>   t <- localTransport
>   node <- newLocalNode t initRemoteTable
>
>   runProcess node $ do
>     self <- getSelfPid
> 
>     greetPid <- spawnLocal greet
>
>     -- Continue with greetings
>     say "Greeting Rudy!"
>     send greetPid (UserIntimation self "Rudy")
>
>     greeting <- expectTimeout 1000000
>     case greeting of
>       Nothing -> die "Greet server not up?"
>       Just g  -> say $ "Greetings says : " ++ g
>
>     -- Wait for all distributed messages to finish exchanging befor exiting
>     liftIO $ threadDelay 1000000

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-cloud-haskell
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

You will notice few peculiar things in the above recipes.

* The data *UserIntimation* is required to be an instance of *Binary*. This is required for serialization between the calling proecss and called process.
* We first create transport. In this case we start the *backend* on local server at port 10501.
* We create a node with the of transport.
* We use *Process* monad to define what we want to do at the node.
* The function *runProcess* takes the *Process* monad and convert it to *IO* action.
* The function *receiveWait* and *match* are used to wrap up kind of messages we expect to run in the *Process*.
* We declare our expectaion using either *expect* or *expectTimeout*. We use *expectTimeout* to showcase that if we do not get message within certain duration, we will start further processing.
* The function *say* is used for logging.




