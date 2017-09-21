<li> Add module definition for *Data.PriorityQueue*.

> module Data.PriorityQueue where

<li> Let us define the *queue* as a sum type. The queue can be either be empty or it can have two sub-queues. We also keep the *rank* information with the queue.

> data Queue a = Empty
>              | Queue Int a (Queue a) (Queue a)
>              deriving Show

<li> Note that the queue is defined as the *heap-ordered*. Automatically it also follows that the values on the right spine are *heap-ordered* as well. Since the priority queue that we are implementing obeys *leftist* property, we will look at a problem where this property would get disturbed. There are two instances when the *leftist* property will be violated,

  <ul>
  <li> when we are inserting a value, and
  <li> when we are deleting a value (i.e. extracting the minimum)
  </ul>

  In both the cases, we will have to find a sub-tree where we are inserting a value (or removing a value). This would result in a possible violation of the *leftist* property. Here we have to take the violated path (where *rank* of the left child is less than the *rank* of the right child), and re-adjust the elements. We will do this by merging two trees.

  Write a function to merge two trees, adjusting the *rank* along the way.

> mergeQs :: Ord a => Queue a -> Queue a -> Queue a
> mergeQs Empty q = q
> mergeQs q Empty = q
> mergeQs left@(Queue _ lv ll lr) right@(Queue _ rv rl rr) =
>   if lv <= rv then
>     swipe lv ll (mergeQs lr right)
>   else
>     swipe rv rl (mergeQs left rr)

  Here the function *swipe* checks the ranks for two trees being merged, and swaps them if they violate *leftist* property. We will implement *swipe* later. 

  <li> Write the function to find *rank* of the queue.

> rank :: Queue a -> Int
> rank Empty = 0
> rank (Queue r _ _ _) = r

  <li> Write the function *swipe* to check the rank and swipe *left* and *right* branches to obey *leftist* property.

> swipe :: a -> Queue a -> Queue a -> Queue a
> swipe v left right =
>   if rank left >= rank right then
>     Queue (rank right + 1) v left right
>   else
>     Queue (rank left + 1) v right left

  <li> Now write the interface functions for our *queue* implementation. We need three functions *insert*, *minimum*, and *deleteMin* to manipulate the queue. But before that, we will need to have helper functions to construct a *queue*.

  <li> Create empty queue.

> emptyQ :: Queue a
> emptyQ = Empty

  <li> Create a singleton queue from a value. The singleton node will have a rank 1.

> singletonQ :: a -> Queue a
> singletonQ v = Queue 1 v Empty Empty

  <li> Implement *insert* function. The *insert* operation is equivalent to merging a singleton into an existing queue.

> insert :: Ord a => a -> Queue a -> Queue a
> insert v q = mergeQs (singletonQ v) q

  <li> Implement *minimum* function. The *minimum* function returns *Nothing* if the queue is empty, else it returns the root value in the tree, which is guaranteed to be minimum.

> minimum :: Queue a -> Maybe a
> minimum Empty = Nothing
> minimum (Queue _ v _ _) = Just v

  <li> Implement *deleteMin* function. We will take out the root value, and will merge two remaining trees.

> deleteMin :: Ord a => Queue a -> Queue a
> deleteMin Empty = Empty
> deleteMin (Queue _ _ l r) = mergeQs l r

