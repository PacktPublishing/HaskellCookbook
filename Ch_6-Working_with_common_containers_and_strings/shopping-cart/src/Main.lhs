Shopping Cart
=============

In this recipe, we will be creating a shopping cart for books. The books are uniquely identified by ISBN numbers. If we add same item, in the shopping cart, we should be able to update existing item or insert a new item in the shopping cart. We will be using Set as a container for shopping items.


How do we do it...
------------------
  <ul>
  <li>Create a new project shopping-cart using simple stack template. 

~~~~
  stack new shopping-cart simple
~~~~

  <li>Open shopping-cart.cabal, add a dependency on 'containers' library in the build-depends subsection of executable subsection.

~~~
  executable shopping-cart
    hs-source-dirs:      src
    main-is:             Main.lhs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , containers
~~~

  <li>Open src/Main.hs, we will add our code here. Import module Data.Set 

> module Main where
>
> import Data.Set as Set

  <li>Create a type to represent a book. The book contains ISBN number, title of the book, and name of the author.

> data Book = Book { isbn :: String
>                  , title :: String
>                  , author :: String }
>                    deriving Show

  <li>Create the equality and ordering of the book solely by looking at ISBN. We cannot use the default order instance, as it will also use other fields to create an order between two books. For our purpose, we will only consider ISBN as primary key for creating an order. 

> instance Eq Book where
>
>   book1 == book2 = isbn book1 == isbn book2
>
> 
> instance Ord Book where
>
>   book1 `compare` book2 = isbn book1 `compare` isbn book2
>

  <li>Now create an item for the shopping cart. Each item contains a book, and the quantity ordered for it. Also define instances of Eq and Ord for the item as we do not want number of books to be considered during the ordering of the items. 

> data Item = Item Book Int deriving Show
>
> instance Eq Item where
>   (Item b1 _) == (Item b2 _) = b1 == b2
>
> instance Ord Item where
>   (Item b1 _) `compare` (Item b2 _) = b1 `compare` b2

  <li>The shopping cart is represented by a set of items.

> type ShoppingCart  = Set Item

  <li>Create an empty shopping cart.

> emptyCart :: ShoppingCart
> emptyCart = Set.empty

  <li>Add a book to the cart. We will create an item with 1 as a default quantity. We will add a new entry if the book is not present in the cart. If the book is already entered into the cart, we will increase the quantity by one. 

> addBook :: Book -> ShoppingCart -> ShoppingCart
> addBook book cart =
>   let item = Item book 1
>       search = Set.lookupGE item cart
>   in case search of
>        Nothing -> Set.insert item cart
>        Just (Item b i) -> if isbn b == isbn book then
>                             Set.insert (Item b (i+1)) cart
>                           else
>                             Set.insert (Item book 1) cart

  <li>Similarly remove a book from the cart, We will reduce the count of the book, if it is already present in the cart. If the count goes down to zero, then we will remove the book from the cart. If the book is not present in the cart, then obviously, we do not need to do anything.

> removeBook book cart =
>   let item = Item book 1
>       search = Set.lookupGE item cart
>   in case search of
>     Nothing -> cart
>     Just (Item b i) -> if isbn b == isbn book then
>                          if 0 >= (i -1) then
>                            Set.delete item cart
>                          else
>                            Set.insert (Item b (i-1)) cart
>                        else
>                          cart

  <li>Now create some books and add them to the cart. We will add few books and remove few books.

> main :: IO ()
> main = do
>   let book1 = Book { isbn = "0262162091"
>                    , author = "Pierce, Benjamin C."
>                    , title = "Types and Programming Languages" }
>
>       book2 = Book { isbn = "8173715270"
>                    , author = "Abelson, Herold et. al."
>                    , title = "Structure and Interpretation of Computer Programs" }
>
>   let cart = emptyCart
>       cart1 = addBook book1 cart
>       cart2 = addBook book2 cart1
>       cart3 = addBook book1 cart2
>       cart4 = addBook book1 cart3
>       cart5 = removeBook book1 cart4
>       cart6 = removeBook book2 cart5
>
>   putStrLn "Empty Cart"
>   print cart
>
>   putStrLn "Add book 1 to cart"
>   print cart1
>
>   putStrLn "Add book 2 to cart"
>   print cart2
> 
>   putStrLn "Add book 1 again"
>   print cart3
>
>   putStrLn "And add book 1 once more"
>   print cart4
>
>   putStrLn "Remove book 1 from cart"
>   print cart5
>
>   putStrLn "Remvoe book 2, this should delete the book from the cart"
>   print cart6

  <li> Build and execute the project

~~~
  stack build
  stack exec -- shopping-cart
~~~

  You should see following output.

  </ul>
How did we do it...
-------------------

The item in a set need to be ordered. And being ordered also forces a definition of equality. Hence we have defined Eq, and Ord instances for our data type. Depending upon the situation, we might want to create the order differently. For example, if one would like, he can make the search based on book title or author. In such a case, a typical trick is to wrap the existing data type, for which Order instance is already defined, in another type, and then defining Order instead for it.

