Creating and Testing Priority Queue
===================================

In this chapter, we have worked with Maps and Sets, imported from library *collections*. Both maps and sets are implemented as binary tree. In this recipe, we will create our own collection **Priority Queue** based on tree, and at the same time we will test it based on its invariants. Many collections, and data structures require binary tree as a basic ingredient. Creating a collection and testing it is a useful exercise for our future applications.

We will be using *QuickCheck* as our testing infrastructure to help write tests for priority queue.

A priority queue that we are going to consider is a leftist heap. A leftist heap is implemented as a *heap-ordered* binary tree. In a *heap ordered* binary tree the value at the node is *less* than or *equal to* the values of children. A priority queue is used where we are always interested in the *minimum* element in the collection, and would like to *extract* or remove it from the collection. The leftist prioirty queue obeys *leftist property*.

The leftist property says that the **rank** of a *left child* is greater than or equal to the **rank** of a right child. The **rank** of a node is the length of the rightmost path from the node to an empty node. This path is called *right spine* of the node. As a result of leftist property, we get a tree where right spine of any node is always shortest path to an empty node.


How do we do it...
------------------
  <ul>

  <li> Create new project *priority-queue* with *default* stack template.

~~~
  stack new priority-queue
~~~
  <li> Delete the file *src/Lib.hs*. Create a directory *src/Data/* and create a new file *src/Data/PriorityQueue.hs*. We will add implementation of priority queue here.

  <li> Open *priority-queue.cabal*. Remove *Lib* from *exposed-modules* subsection from the section *library*. Replace this by our new module *Data.PriorityQueue*. 

~~~
library
  hs-source-dirs:      src
  exposed-modules:     Data.PriorityQueue
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
~~~
  <li> Open *src/Data/PriorityQueue.hs*. We will implement priority queue here.

  <li> **include the Data.PriorityQueue module**

  <li> We are now done with implementation of priority queue. Open *app/Main.hs*, we will use priority queue with some data.

  <li> Import *Data.PriorityQueue* module

> import Data.PriorityQueue as Q

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> In the *main* function, create an empty queue, and keep adding few integers. 

> main :: IO ()
> main = do
>   let e = emptyQ :: Queue Int
>       q1 = insert 10 e
>       q2 = insert 20 q1
>       q3 = insert 15 q2
>       q4 = insert 2 q3
>   -- This should print 2 (minimum value)
>   print (Q.minimum q4)
>   -- This should remove minimum
>   let q5 = deleteMin q4
>   -- This should now print 10
>   print (Q.minimum q5)

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- priority-queue
~~~

  You should see following output,

  <li> **Include quick test code here**

  </ul>


How did we do it...
-------------------

We implemented a leftist tree here. This is a very good example of immutability, data persistence and in general how to implement a data structure in Haskell. We started with a representation for a queue, *Heap* and an invariant *Leftist Property*. A typical leftist tree looks like the one in the image below -

**<<insert leftist tree here>>**

Then we proceeded to implement a function *mergeQs* which is at the heart of the implementation. The operations, *insert* and *deleteMin* both result in an operation that changes the structure of the tree. This change can violate the leftist property or the heap order. The *mergeQs* function merges two trees and restores these invariants.

Most importantly, we tested our queue with *QuickCheck*, a generative approach towards testing. The *QuickCheck* works by generating random data, and tries to zoom onto a problem in case of a failure, by generating smaller set of data. This is very helpful, and it is possible to catch *subtle* bugs with this approach. It also removes developer's bias.

At the heart of *QuickCheck* is the class of *Arbitrary* which has a function *arbitrary* to generate random instances of our data types. The core of *QuickCheck* comes from the following definition of *Testable*

~~~haskell
class Testable prop where
  property :: prop -> Property
~~~

The data type *Property* is the result of testing *prop*. The real genius of *QuickCheck* comes from the following instance of *Testable*.

~~~haskell
instance [safe] (Arbitrary a, Show a, Testable prop) =>
                Testable (a -> prop)
~~~

The above instance tells that if we have a testable property, and an *Arbitrary* instance of *a*, then we can test a function *a -> prop*. This fantastic definition helped us test our functions such as *verifyLeftist :: Queue a -> Bool*. The *Bool* is a testable property. And thus, our function also becomes testable. The *QuickCheck* library will generate arbitrary instances of *Queue a* and run the tests!


