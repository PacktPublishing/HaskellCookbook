User Cloud Haskell to start master and slave nodes
==================================================

In this recipe, we will use *simplelocalnet* to create *master* and *slave* nodes. We will start *slave* nodes and *master* node, and use *master* node to get knowledge about slave nodes.

How do we do it...
------------------
  <ul>

  <li> Create new project *master-slave* with *simple* stack template.

~~~
  stack new master-slave simple
~~~

  <li> Add dependency on *distributed-process* and *distributed-process-localnet* libraries in the *build-depends* sub-section of *executable* section.

~~~
  executable master-slave
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , distributed-process
                       , distributed-process-simplelocalnet
~~~

  Note that you might have to add the dependent library specifically to *extra-deps* section as these libraries are not part of *stackage LTS* yet at the time of writing this recipe. Add following to *stack.yaml*.

~~~
extra-deps:
  - distributed-process-0.6.6
  - distributed-process-simplelocalnet-0.2.3.3
  - syb-0.6
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add *Main* module, and relevant imports.

> module Main where
>
> import System.Environment (getArgs)
> import Control.Distributed.Process
> import Control.Distributed.Process.Node
> import Control.Distributed.Process.Backend.SimpleLocalnet
> 

  <li> Create the *Process* that we will run on *master* node.

> masterTask :: Backend -> [NodeId] -> Process ()
> masterTask backend slaves = do
>   liftIO $ putStrLn $ "Initial slaves: " ++ show slaves
>   terminateAllSlaves backend

  <li> Use *main* to either start *master* or *slave* node.

> main :: IO ()
> main = do
>   args <- getArgs
>
>   case args of
>
>     "-m":h:p:[] -> do
>       backend <- initializeBackend h p initRemoteTable
>       startMaster backend (masterTask backend)
>     "-s":h:p:[] -> do
>       backend <- initializeBackend h p initRemoteTable
>       startSlave backend
>   

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- master-slave -s 127.0.0.1 10501 & 
  stack exec -- master-slave -s 127.0.0.1 10502 &
  stack exec -- master-slave -s 127.0.0.1 10503 &
  stack exec -- master-slave -s 127.0.0.1 10504 &
  stack exec -- master-slave -m 127.0.0.1 10505
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

In this recipe, we use *simplelocalnet* backend to start both *master* and *slave* nodes. It is possible to use different backends such as *p2p* or *azure* backend. When we are closing the *master*, we close all *slave* nodes. 

