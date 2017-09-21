Working with Predefined Lenses
==============================

In this recipe, we will work with predefined lenses. We will use a library *lens-aeson* for dealing with JSON data. The *aeson* library is a popular library for parsing and writing JSON instances for the user defined data. Many a times, we have to deal with JSON, and parse it on the fly to extract the desired data.

In this recipe, we will use generic JSON parser provided by *aeson*, and then use *lens-aeson* to dig through the JSON for extracting the data.

How do we do it...
------------------
  <ul>

  <li> Create new project *working-with-aeson-lens* with *simple* stack template.

~~~
  stack new working-with-aeson-lens simple
~~~

  <li> Add dependency on *lens* library in the *build-depends* sub-section of *executable* section. Also add dependency on *aeson* and *lens-aeson* libraries. Also add *bytestring* and *text* as *aeson* uses *bytestring* for parsing, *text* for string values. Add *vector* as we need to work with arrays in JSON.

~~~
  executable working-with-aeson-lens
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , lens
                       , lens-aeson
                       , aeson
                       , bytestring
                       , text
                       , vector
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Use *OverloadedStrings* extension and define *Main* module. Import the required modules. 

> {-# LANGUAGE OverloadedStrings #-}
>
> module Main where
>
> import Control.Lens
> import Data.Aeson.Lens
> import Data.Aeson
> import Data.ByteString.Lazy.Char8 as BC
> import Data.Vector hiding ((++))
> import Data.Text

  <li> Let us consider a decoded JWT token (https://jwt.io/). We have added few permissions to the JWT token.

> jwtToken :: ByteString
> jwtToken = "{ \"header\" : { \"alg\": \"HS256\", \"typ\": \"JWT\" }, \"payload\" : { \"sub\": \"1234567890\", \"name\": \"John Doe\", \"admin\": true, \"permissions\": [ \"status\", \"user:read\", \"user:write\" ]} }"

  <li> Now find the attributes for different fields 

> isJWT :: ByteString -> Bool
> isJWT tok = case (decode tok :: Maybe Value) of
>               Nothing -> False
>               Just v  -> let typ = v ^? key "header" . key "typ"
>                          in typ == Just (String "JWT")

  <li> In fact, we need not explicitly pass the JSON data.

> isAdmin :: AsValue v => v -> Bool
> isAdmin tok = (tok ^? key "payload" . key "admin") == Just (Bool True)

  <li> We can also access an element of an array.

> permission0 :: AsValue v => v -> Maybe Value
> permission0 tok = (tok ^? key "payload" . key "permissions" . nth 0)

  <li> Get all the permissions.

> permissions :: AsValue v => v -> [Text]
> permissions tok = Prelude.head (tok ^. key "payload" . key "permissions" . _Array . to toList ^.. below Data.Aeson.Lens._String)

  <li> Use the token function to test our tokens to print permissions, admin access and type of token.

> main :: IO ()
> main = do
>   BC.putStrLn "Analyzing token"
>   BC.putStrLn jwtToken
>
>   Prelude.putStrLn $ "Is JWT Token? " ++ (show $ isJWT jwtToken)
>   Prelude.putStrLn $ "Is Admin? " ++ (show $ isAdmin jwtToken)
>
>   Prelude.putStrLn $ "Permissions = " ++ (show $ permissions jwtToken)

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- working-with-aeson-lens
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------

This recipe shows just one aspect of usefulness. Here we have used the *lens*es created for JSON using *aeson* library. This helps us parse arbitrary JSON without actually writing an instance of *FromJSON* or *ToJSON* required to represent the user defined data. The *lens* can be similarly used for simplifying manipulation of large data structure required for certain libraries.
