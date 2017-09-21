Using Closure to Communicate between Nodes
==========================================

In this *recipe*, we will start two separate processes, one master, and one slave. We will use *master* process to spawn a subprocess on *slave* node.

How do we do it...
------------------
  <ul>

  <li> Create new project *master-slave* with *simple* stack template.

~~~
  stack new using-closure simple
~~~

  <li> Add dependency on *distributed-process* and *distributed-process-localnet* libraries in the *build-depends* sub-section of *executable* section.

~~~
  executable using-closure
    hs-source-dirs:      src
    main-is:             Main.hs
    ghc-options:         -threaded
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , distributed-process
                       , distributed-process-simplelocalnet
~~~
  At the time of writing this recipe, some of the dependencies are not resolved through *stackage LTS*. Add following to *stack.yaml*.

~~~
extra-deps:
  - distributed-process-0.6.6
  - distributed-process-simplelocalnet-0.2.3.3
  - syb-0.6
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add *Main* module, and relevant imports. Enable *TemplateHaskell*

> {-# LANGUAGE TemplateHaskell #-}
> module Main where
>
> import System.Environment (getArgs)
> import Control.Distributed.Process
> import Control.Distributed.Process.Closure
> import Control.Distributed.Process.Node
> import Control.Distributed.Process.Backend.SimpleLocalnet
> import Control.Monad
> import Control.Concurrent
> 

  <li> Create a *Process*. This process recives message, and returns an acknowledgment.

> ack :: Int -> Process ()
> ack i = do
>   self <- getSelfPid
>   say $ "Started the process at " ++ show self
>   forever $ do
>     receiveWait [match (acknowledge self), matchAny (\_ -> say "Message received") ]
>
>   where
>     acknowledge :: ProcessId -> (ProcessId, String) -> Process ()
>     acknowledge self (pid, message) = do
>       liftIO $ threadDelay (i*1000*1000)
>       send pid $ ("Ack from : " ++ show self ++ ", message : " ++ message :: String)

  <li> Create a remote table. This table has entries that enables cloud haskell to spawn a process remotely.

> remotable ['ack]

  <li> Create the master table. The *remoteable* macro creates the table in the current module. Let's combine the *initRemoteTable* with above table. 

> masterTable :: RemoteTable
> masterTable = Main.__remoteTable initRemoteTable

  <li> Create the *Process* that we will run on *master* node. First create a closure around *ack*, so that we can serialize the *Process* alongwith any arguments.

> ackClosure :: Int -> Closure (Process ())
> ackClosure = $(mkClosure 'ack)
>
> masterTask :: Backend -> [NodeId] -> Process ()
> masterTask backend slaves = do
>   liftIO $ putStrLn $ "Initial slaves: " ++ show slaves
>   self <- getSelfPid
>   case slaves of
>     [] -> liftIO $ putStrLn $ "No slaves"
>     (s:_) -> do
>       pid <- spawn s $ ackClosure 1
>       say $ "Spawned " ++ show pid ++ " on " ++ show s
>       send pid (self, "Remote confirmation" :: String)
>       m <- expectTimeout (1000000*3)
>       case m of
>         Nothing -> say "No message confirmation from remote node"
>         Just r  -> say $ "Remote confirmation: " ++ r
>   terminateAllSlaves backend

  <li> Use *main* to either start *master* or *slave* node. 

> main :: IO ()
> main = do
>   args <- getArgs
>
>   case args of
>
>     "-m":h:p:[] -> do
>       backend <- initializeBackend h p masterTable
>       startMaster backend (masterTask backend)
>     "-s":h:p:[] -> do
>       backend <- initializeBackend h p masterTable
>       startSlave backend
>   

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- master-slave -s 127.0.0.1 10501 & 
  stack exec -- master-slave -m 127.0.0.1 10502
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In this recipe, we use *simplelocalnet* backend to start both *master* and *slave* nodes. We then created a table using template haskell based macro *remoteable*. In this table, we registered all the functions (resulting in *Process*) that need to be seralized across.

The *Cloud-Haskell* uses *static pointer* extension, which allows *cloud haskell* to find a fingerprint of a function and then compose the *serializable* arguments alongwith the *fingerprint*. On the remote node, using the information contained in the closure, it is possible for the remote node to recreate the *function call* with arguments. The *static pointer* extension, however can be used only for rank-1 types. For higher ranks, *cloud haskell* converts it in to *Data.Dynamic* type so that it can cast it back into original function on the remote node. 
