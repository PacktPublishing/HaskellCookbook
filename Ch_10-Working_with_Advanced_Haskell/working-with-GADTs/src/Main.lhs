Working with GADTs
==================

We will work with GADTs (Generalized Algebraic Data Types). GADTs extend the data constructors, and allow us more expressivity for representiing a complex structure like a DSL. In this recipe, we will use GADTs to create an expression representation, and a simple parser.


How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-GADTs* with *simple* stack template.

~~~
  stack new working-with-GADTs simple
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> Enable *GADTs*, and *StandaloneDeriving*

> {-# LANGUAGE GADTs, StandaloneDeriving #-}
> module Main where
>
> import Control.Monad
> import Data.Char
> import Control.Applicative

  <li> GADTs take algebraic data type one step further, and allow us to write data constructors explicitly. For example we can represent a set of expressions as -

> data Expr where
>   Value :: Int -> Expr
>   Add :: Expr -> Expr -> Expr
>   Mult :: Expr -> Expr -> Expr
>
> deriving instance Show Expr 

  <li> We can evaluate the above expression as -

> eval :: Expr -> Int
> eval (Value i) = i
> eval (Add e1 e2) = eval e1 + eval e2
> eval (Mult e1 e2) = eval e1 * eval e2

  <li> Create soem expression

> sampleExpr :: Expr
> sampleExpr = Add (Value 10) (Mult (Add (Value 20) (Value 10)) (Value 20))

  <li> In fact, we can also represent parser monad with GADTs

> data Parser a where
>   Return :: a -> Parser a
>   Unparser :: (String -> [(a,String)]) -> (a -> Parser b) -> Parser b
> 

  <li> Instantiate *Functor*, *Applicative* and *Monad* instance for our *Parser*.

> instance Functor Parser where
>
>   fmap f (Return x) = Return (f x)
>   fmap f (Unparser parseFn afb) = Unparser parseFn (fmap f . afb) 
>   

> instance Applicative Parser where
>
>   pure = Return
>
>   Return f <*> Return x = Return (f x)
>   Return f <*> Unparser parseFn cfa = Unparser parseFn (fmap f . cfa)
>   Unparser pc cfab <*> pa = Unparser pc (\c -> cfab c <*> pa)

> instance Monad Parser where
>
>   return = Return
>
>   Return x >>= f = f x
>   Unparser parseFn afb >>= f = Unparser parseFn ((>>= f) . afb)
>   

  <li> Similar to *Expr*, we can also write an evaluator for our *Parser*.

> parse :: Parser a -> String -> [(a, String)]
> parse (Return x) s = [(x, s)]
> parse (Unparser parseFn afb) s =
>   case parseFn s of
>     (a,s'):_ -> parse (afb a) s'
>     _        -> []

  <li> Write a set of parsing functions. Create a *digit* parser.

> conditional :: (Char -> Bool) -> String -> [(Char,String)]
> conditional _ [] = []
> conditional f (x:xs) | f x = [(x,xs)]
> conditional _ _ = []

> digit :: Parser Char
> digit = Unparser (conditional isDigit) Return 

  <li> Use above functions in *main* to evaluate *Expr* and run *Parser*.

> main :: IO ()
> main = do
>   putStrLn $ "Sample Expression - " ++ (show sampleExpr) ++ " = " 
>   print $ eval sampleExpr
>
>   -- Create a parser for ditit
>   putStrLn "Parsing digit from \"1abc\" should be successful"
>   print $ parse digit "1abc"
>
>   putStrLn "Parsing digit from \"abc\" should fail"
>   print $ parse digit "abc"
> 

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-GADTs
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

We have looked at Generalized Algebraic Data Types or *GADTs*. It allows us to create a domain specific language, nad evaluate it the way we want it. We can use this DSL to simulate, or execute or any other purpose.

In the second part, we have created a *Parser* with GADTs. In fact we can generalise it to a generic monad,

```haskell
data Parser a where
  Return :: a -> Parser a
  Unparser :: (String -> [(a,String)]) -> (a -> Parser b) -> Parser b
```

Here the data constructors *Return* and *Unparser* look very similar to *return* and bind *>>=* of a *Monad*. In fact, we can actually represent a generic instance of a monad in terms of data constructors. This is equivalent to *Free* monad. For more information, have a look at *(https://www.andres-loeh.de/Free.pdf)*.
