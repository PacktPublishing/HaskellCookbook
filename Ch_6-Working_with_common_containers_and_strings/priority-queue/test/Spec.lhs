  <li> We will add the tests to verify our claims about invariants in priority queue. We will use the library *QuickCheck* to verify our claims. Open *priority-queue.cabal* and add the dependency *QuickCheck* to the *build-depends* sub-section of the section *test-suite*. Note that the *stack* has already added the dependency on our library *priority-queue* here.

~~~
test-suite priority-queue-test
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  build-depends:       base
                     , priority-queue
                     , QuickCheck
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
~~~

  <li> Import the *QuickCheck* module.

> import Test.QuickCheck
> import qualified Data.PriorityQueue as Q

  <li> The *QuickCheck* library, contrary to other unit test libraries, generates tests. Here we write *properties* that should be verified by the *QuickCheck*. This saves us from a developer's bias, and at the same time, we get a minimum input (or steps) that would fail the property.

  <li> We need to generate arbitrary instances of Queue. Let's use *QuickCheck*'s infrastructure class *Arbitrary* to do that. We take a list of values, and generate a queue out of it using *Gen* monad. The *Arbitrary* instance of *Queue* first generates the list of values, and then *inserts* these values into a queue through *foldr*. 

> qFromList :: Ord a => [a] -> Gen (Q.Queue a)
> qFromList xs = return (foldr Q.insert Q.emptyQ xs)
>
> instance (Arbitrary a, Ord a) => Arbitrary (Q.Queue a) where
>   arbitrary = listOf arbitrary >>= qFromList

  <li> We will first verify the claim about the *leftist* property of the tree. To be able to do that we write a function verify, that takes a tree, and verifies if each node follows the *leftist* property. We will also verify the *rank* stored in the queue in the process. 

> qrank :: Q.Queue a -> Int
> qrank Q.Empty = 0
> qrank (Q.Queue _ _ l r) = 1 + minimum [qrank l, qrank r]
>
> verifyLeftist :: Q.Queue a -> Bool
> verifyLeftist Q.Empty = True
> verifyLeftist q@(Q.Queue rnk v l r) =
>   and [ qrank q == rnk
>       , qrank l >= qrank r
>       , verifyLeftist l
>       , verifyLeftist r ]

  <li> Now test the *heap ordered* property of the queue.

> heapOrdered :: Ord a => Q.Queue a -> Bool
> heapOrdered Q.Empty = True
> heapOrdered (Q.Queue _ _ Q.Empty Q.Empty) = True
> heapOrdered (Q.Queue _ v Q.Empty r@(Q.Queue _ rv _ _)) =
>   and [ v <= rv, heapOrdered r ]
> heapOrdered (Q.Queue _ v l@(Q.Queue _ lv _ _) Q.Empty) =
>   and [ v <= lv, heapOrdered l ]
> heapOrdered (Q.Queue _ v l@(Q.Queue _ lv _ _) r@(Q.Queue _ rv _ _)) =
>   and [ v <= lv, v <= rv, heapOrdered l, heapOrdered r]

  <li> Now we have two properties *verifyLeftist* and *heapOrdered*, which take a queue, and returns a boolean. We will use these properties in conjunction with *quickCheck* to run the tests. 

> main :: IO ()
> main = do
>   putStrLn ""
>   putStrLn "Verifying Leftist Property"
>   quickCheck (verifyLeftist :: Q.Queue Int -> Bool)
>   putStrLn "Verifying Heap Ordered Property"
>   quickCheck (heapOrdered :: Q.Queue Int -> Bool)

  <li> Build and execute the test. Run the *stack build* command with *--test* argument.

~~~
  stack build -- test
~~~

  You should be able to see following output

  **<<<insert test output here>>>**

  <li> The above test result shows that it ran 100 tests with each property. We can introduce a bug (this time, in our testing code by modifying the *qrank* function not to add *1* to the children ranks. Define qrank as

~~~haskell
  qrank (Q.Queue _ _ l r) = minimum [qrank l, qrank r]
~~~

  If you now run the test, then you should see something like this! You should also see the input queue for which the test failed. This is really helpful and shows the strength of *QuickCheck*.

~~~
  Verifying Leftist Property
  *** Failed! Falsifiable (after 3 tests):  
  Queue 1 (-2) (Queue 1 0 Empty Empty) Empty
  Verifying Heap Ordered Property
  +++ OK, passed 100 tests.
~~~


