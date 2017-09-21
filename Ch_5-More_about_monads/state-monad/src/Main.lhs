 In this module we are implementing a state monad from the basic
 principles. Let us start by creating a module Main, we will add our
 state monad and the example with it. 

> module Main where
>
> import Prelude hiding (Either(..))
> import Data.Functor
> import Control.Applicative
> import Control.Monad

 Now add the definition for the state monad. A state monad will store
 the state 's' with the monad. The monad itself can produce any
 output. 

> data State s a = State { runState :: s -> (a, s) } 

 The state data type is defined as a function which takes in some
 state 's' as an input, and produces an output (a,s) which includes
 the state 's', and some output 'a'.

 Now we will write the Functor instance for the state.

 Writing Functor instance is easy, we need to transform the output 'a'
 with the function 'f :: a -> b' and produce 'b'. 

> instance Functor (State s) where
>   fmap f (State stateFunc) =

 The fmap implementation involves running the input state monad to
 produce 'a', and then applying function 'f' on it. 

> 
>     let nextStateFunction s =
>           let (xa, s1) = stateFunc s
>           in (f xa, s1)
>     in State nextStateFunction

 Now we will write applicative instance for the state type.

> instance Applicative (State s) where

 Write pure instance for the applicative. It involves introducing the
 given input and introduce it in the state data type.

 The pure implementation involves grabbing the input 'a' to pure
 function, and returning the state function which returns state 's'
 along with input 'a'.

>   pure x = let pureState s = (x, s) in State pureState
> 

 The applicative function <*> involves taking in the 'State s (a -> b)'
 and applying it on 'State s a' to produce 'State s b'.  

>   sf <*> sa = let stateFunc s =

 Apply the state function with the input 's' on 'sf' i.e. 'State s (a
 -> b)' first. We will retrieve the state 's1' in this step, along
 with the function 'f :: a -> b' in this step.

>                     let (f, s1) = runState sf s

 Then apply the state function on 'State s a'. Use the state 's1' and
 then apply it to get 'a' with state 's2'

> 
>                         (a, s2) = runState sa s1

 Apply 'f' to 'a' to produce b, and use 's2' as the resultant state. 

>                     in (f a, s2)
>               in State stateFunc

 Now we define the monad instance for the state.

> instance Monad (State s) where
>
>   return = pure
>
>   sa >>= fsb = let stateFunc s = 
>                      let (a, s1) = runState sa s
>                      in runState (fsb a) s1
>                in State stateFunc


 We will now define two functions 'get' to get the current state, and
 'put' to put some state 's' into the state data type. Note that, we
 have to do that by remaining in the State data type itself.

> get :: State s s
> get = let stateFunc s = (s, s)
>       in State stateFunc

 Write the put function, it involves taking some state 's', and
 replacing it in the current state. We may not need any output, and
 just ignore the output. 

> put :: s -> State s ()
> put s = let stateFunc _ = ((), s)
>         in State stateFunc


 Use the state monad that we have just created. We create a data type
 for cursor to represent the position of the cursor on the screen. The
 cursor position is a cartesian coordinate (x, y). (0, 0) denotes the
 left-top position on the screen.

> data Cursor = Cursor Int Int deriving Show

 With cursor, we can define the cursor movements.

> data Move = Up Int | Down Int | Left Int | Right Int deriving Show

 We can apply a movenent to the cursor

> apply :: Cursor -> Move -> Cursor
> apply (Cursor x y) (Up i)    = Cursor x (y-i)
> apply (Cursor x y) (Down i)  = Cursor x (y+i)
> apply (Cursor x y) (Right i) = Cursor (x + i) y
> apply (Cursor x y) (Left i)  = Cursor (x -i) y

 Suppose we have a list of movements (through keyboard or mouse) and
 would like to move cursor accordingly. This we can achieve using
 state monad.

> applyMoves :: [Move] -> State Cursor ()
> applyMoves [] = return ()
> applyMoves (x:xs) = do
>   cursor <- get
>   let newcursor = apply cursor x
>   put newcursor
>   applyMoves xs

 Finally, let's now try by applying the moves to the initial cursor
 position (0,0). Let's apply four moves Down 100, Right 100, Up 100,
 and Left 100, basically returning back to the same position.

> moves :: [Move]
> moves = [Down 100, Right 100, Up 100, Left 100]

 Now let's put this together and get the final state. First apply all
 the moves with applyMove function, and then run the resultant state
 with initial state (Cursor 0 0)

> resultCursor :: ((), Cursor)
> resultCursor = runState (applyMoves moves) (Cursor 0 0)

 We might also try arbitrary cursors, the moves, when applied should
 return the function back to the same position. 

> resultCursor1 :: ((), Cursor)
> resultCursor1 = runState (applyMoves moves) (Cursor 12 12)

 Now test the result cursor

> main :: IO ()
> main = do
>   print (snd $ resultCursor)
>   print (snd $ resultCursor1)

