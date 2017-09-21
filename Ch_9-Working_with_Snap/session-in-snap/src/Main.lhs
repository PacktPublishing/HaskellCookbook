
Session in Snap
===============

In this recipe, we will be working with *SessionManager* in Snap. HTTP is a connection-less protocol, and the concept of session has to be built on the top of interaction between client and server interaction. The session is usually represented by a some *key-value* pair that can be persisted across the interactions between a client and the server. In HTTP, this can be done in multiple ways, one of the most popular way of handling session is to set the session cookies. The session cookies are retained by the browser for a particular interaction duration.

In this recipe, we will set session cookies through session manager in Snap.


How do we do it...
------------------
  <ul>

  <li> Create new project *session-in-snap* with *simple* stack template.

~~~
  stack new session-in-snap simple
~~~

  <li> Add dependency on *snap-core* library in the *build-depends* sub-section of *executable* section. Add the other libraries necessary for working with *Snap*.

~~~
  executable session-in-snap
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , snap-core
                       , snap-server
                       , snap
                       , lens
                       , bytestring
                       , text
~~~

  <li> Use following command to solve the constraints within the current resolver. For this recipe, we have used "lts-9.1" as a resolver.

~~~
  stack --resolver lts-9.1 solver --update-config
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. Enable GHC extensions *OverloadedStrings* and *TemplateHaskell*. After the *Main* module definition, add the necessary imports.

> {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
> module Main where
>
> import Snap
> import Snap.Core
> import Snap.Http.Server
> import Snap.Snaplet.Session
> import Snap.Snaplet.Session.Backends.CookieSession
> import Control.Lens
> 

  <li> Create our own application that nests session manager. 

> data MyApp = MyApp { _session :: Snaplet SessionManager } 

  <li> Use lens macros to create a lens for our application.

> makeLenses ''MyApp

  <li> Write a handler to print "Hello World!" as a text response. In addition to printing a message, we also use *withSession* function to use the session inside our handler. We use the handler also to set a key "user" inside the session. 

> greetings :: Handler MyApp MyApp ()
> greetings = withSession session $ do
>   with session $ setInSession "user" "Haskell Web Developer"
>   writeBS "Hello World"

  <li> Write an handler where we get the key stored in the session and show it as an output.

> welcome :: Handler MyApp MyApp ()
> welcome = withSession session $ do
>   message <- with session $ do
>     name <- getFromSession "user"
>     return $ maybe "You are not registered" id name
>   writeText $ message
>   writeText "\n"

  <li> Initialize the application, by providing session manager. Initialize the cookie manager and embed it as a session manager in our application, *MyApp*. Add two routes *greet* and *welcome* for functions *greetings* and *welcome* respectively. 

> initMyApp = makeSnaplet "sessionDemo" "Demonstrating session with Snaplet" Nothing $ do
>   sess <- nestSnaplet "session" session $ initCookieSessionManager "site_key.txt" "demo-session" Nothing (Just 3600)
>   addRoutes [ ("/greet", greetings)
>             , ("/welcome", welcome)]
>   return (MyApp sess)

  <li> Create a file called "site_key.txt" in the project folder. Add a secret passphrase in the file, and save the file. This file will be used as a key for encrypting the session contents. Now serve the snaplet through the function *serveSnaplet*. 

> main :: IO ()
> main = serveSnaplet defaultConfig initMyApp

  <li> Build and execute the project. The server should run at the port 8000. 

~~~
  stack build
  stack exec -- session-in-snap
~~~

  Open the browser, and point it to *http://localhost:8000/welcome*, you should see a message, *You are not registered user*. as shown below - 

  Now visit *http://localhost:8000/greet*. This should set the cookie in the session. If you now visit *http://localhost:8000/welcome*, you should see the message, *Haskell Web Developer*. 

  </ul>


How it works...
-------------------

In the above recipe, we have taken following steps to store information in session -

* *withSession* function introduces *session* lens into the handler. At the end of the request, the *withSession* function commits the changes to the session. 
* *with* function allows us to use *session* functions in the handler.
* The function *setInSession* allows us to set a key to the value that we would like.
* The function *getFromSession* allows us to get a key if it is present in the session.
* We use *initCookieManager* to initialize cookie based session manager supplied with snap framework. The cookie manager is configured with a "site-key.txt" a file containing a private encoding key for session manager, the name of the session key, and expiry time for the session. 

You can use curl to see the cookie generated. Start the snap server as mentioned in the previous section, and connect to the "greet" end point using following command,

~~~
  curl -X GET http://localhost:8000/greet --verbose --cookie-jar cookies.txt
~~~

You should see following output.


The cookie that is acquired from the above interaction is stored by user client such as *curl* and *browser*. This session cookie is shared again with the server when interacting next time with the server. Hence, next time when you connect with the end point *http://localhost:8000/welcome* the session cookie is decoded and the embedded message is displayed.

To run the same end point with *curl*, run the command

~~~
  curl -X GET http://localhost:8000/welcome --verbose -b cookies.txt --cookie-jar cookies.txt
~~~

