
Working with Text and Bytestring
================================

In this recipe, we will be looking at alternative representations of *String*. The *String* is a list of *Char* and is not an efficient implementation. **text** and **bytestring** are most popular packages for alternative and efficient string implementations. While **text** implements *Unicode* characters, **bytestring** is good for binary data. In this recipe, we will work with these data types, and convert them into each other, and also explore and GHC extension for strings. 


How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-text-and-bytestring* with *simple* stack template.

~~~
  stack new working-with-text-and-bytestring simple
~~~

  <li> Add dependency on *text* and *bytestring* libraries in the *build-depends* sub-section of *executable* section.

~~~
  executable working-with-text-and-bytestring
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , text
                       , bytestring
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li> At the top of the file, add following pragma for using overloaded strings.

> {-# LANGUAGE OverloadedStrings #-}

  This is the way an extension is enabled for GHC compiler. This particular extension allows us to use string of types *String*, *Text* and *ByteString*. In fact, you will notice that all these data types are instances of class *IsString*. If a data type is an instance of *IsString* then the extension *OverloadedStrings* can be applied in the context of that data type. One of the advantage of using *OverloadedStrings* extension is that we can use quoted string without having to explicitly converting from builtin *String* data type. We are going to see this in action when we are dealing with *Text* and *ByteString*.

  Add following modules for working with *Text* and *ByteString*.

> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO
> import qualified Data.ByteString.Char8 as B
> import qualified Data.Text.Encoding as TE
> import System.IO

  <li> Create bytestring, text

> bString :: B.ByteString
> bString = "This is a bytestring"

  Create text

> tString :: T.Text
> tString = "This is a text string"

  <li> Convert from string to text and bytestrings

> stringToByteString :: B.ByteString
> stringToByteString = B.pack "Converted from string to bytestring"
>
> stringToText :: T.Text
> stringToText = T.pack "Converted from string to text"

  <li> Convert from bytestring and text to string

> bytestringToString :: String
> bytestringToString = B.unpack "From bytestring to string"
>
> textToString :: String
> textToString = T.unpack "From text to string"

  <li> Converting between bytestring and text

> bytestringToText :: T.Text
> bytestringToText = TE.decodeUtf8 "From bytestring to text"
>
> textToBytestring :: B.ByteString
> textToBytestring = TE.encodeUtf8 "From text to bytestring"

  <li> List like operations on text and bytestring. Most of the operations on string works on text and bytestring too.

> textHead :: Char
> textHead = T.head "First" -- returns 'F'
>
> textTail :: T.Text
> textTail = T.tail "First" -- returns 'F'
>
> byteHead :: Char
> byteHead = B.head "First" -- should get "irst"
>
> byteTail :: B.ByteString
> byteTail = B.tail "First" -- should get "irst"
  

  <li> Write *main* function. In this recipe, we will not be using all the above functions in main. Instead we will only print bytestring and text using *putStrLn*. The *text* and *bytestring* modules define their own versions of *System.IO* functions. In the following function, we will print *ByteString* and *Text* respectively. 

> main :: IO ()
> main = do
>   TIO.putStrLn tString
>   B.putStrLn bString
>   -- Open a file and write both the strings into the same file.
>   withFile "text-out.txt" WriteMode $ \h -> do
>     TIO.hPutStrLn h tString
>     B.hPutStrLn h bString

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-text-and-bytestring
~~~

  You should see following output,

  The program should also write an output to *text-out.txt* writing the same lines.

  </ul>


How did we do it...
-------------------

The *Text* and *ByteString* are very efficient implementations of *String*. We have already used *ByteString* in the recipe where we used *Attoparsec* to parse the INI files. And we will be using these types in many recipes to come. 

