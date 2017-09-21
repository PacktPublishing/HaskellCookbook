Working with Iso
================

So far, we have looked at *lens* and *traversals* which are aimed at focusing on a particular field(s) from the context of accessing or changing its value. In this recipe, we will look at *Iso* which represents isomorphism between two types. It is possible to go back and forth between two types. For example, we can convert from *Text* to *String* and vice-a-versa.  

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-iso* with *simple* stack template.

~~~
  stack new working-with-iso simple
~~~

  <li> Add dependency on *lens* library in the *build-depends* sub-section of *executable* section. Also add dependency on  *text* and *bytestring*. Also add *quicklz* library for compression utility.

~~~
  executable working-with-iso
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , lens
                       , text
                       , bytestring
~~~

  Note that, at the moment of writing this recipe, the library *quicklz* are not included in the stack package repository. We need to add them explicitly to the *stack.yaml* in the project directory. Explicitly add following packages in the *extra-deps* section in *stack.yaml*. We have used **lts-9.0** resolver for package resolution for this recipe.

~~~yaml
extra-deps:
  - quicklz-1.5.0.11
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Add *OverloadedStrings* extension before writing *Main* module definition.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Control.Lens
> import Data.Text.Strict.Lens
> import qualified Codec.Compression.QuickLZ as LZ
> import qualified Data.ByteString.Char8 as B
> import Data.ByteString.Lens

  <li> Use *Iso* to convert between *text*, *string* 

> stringConvertTest :: IO ()
> stringConvertTest = do
>   let str = "A string to text" :: String
>       text = str ^. packed
>       str1 = text ^. unpacked
>   -- You can also do (from packed)
>   let str2 = text ^. (from packed)
>
>   putStrLn $ "String -> Text -> String round trip successful? " ++ show (str == str2)

  <li> Create a lens for compression and decompression of a *ByteString*.

> compress :: Iso' B.ByteString B.ByteString
> compress = iso LZ.compress LZ.decompress

  This represents a one to one correspondence between original string, its compression and uncompressed string

  <li> Now convert string to bytestring. Compress it, and then back to string by uncompressing it.

> strCompressRoundTrip :: String -> String
> strCompressRoundTrip s = s ^. (packedChars . compress . from compress . unpackedChars)

  <li> Now test it with some messages.

> -- Sample string for compression
> message :: String
> message = "The quick brown fox jumps over the lazy dog"

> strCompressRoundTripTest :: IO ()
> strCompressRoundTripTest = do
>   let str1 = strCompressRoundTrip message
>   putStrLn ("Compressing and uncompressing \"" ++ message ++ "\"")
>   putStrLn ("Test Successful? " ++ show (str1 == message))

  <li> Use the *iso*s and *lens*es in the *main* function.

> main :: IO ()
> main = do
>   stringConvertTest
>   strCompressRoundTripTest

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-iso
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

*Iso* represent an isomorphic lens. It represents a bidirectional lens. So if you can use *Iso* *compress* to convert from *ByteString* to compressed *ByteString*, then you can use *from compress* to convert compressed *ByteString* back to *String*. An *Iso* is also a lens, and it is possible to combine with other lenses as we have done in the case of compress.

*Iso*s are more convenient if we often convert between different string types (*text*, *bytestring*, and *String*).
