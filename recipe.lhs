# -*- mode: snippet -*-
# name: recipe-cookbook
# expand-env: ((yas-indent-line 'fixed) (yas-wrap-around-region 'nil))
# key: recipe
# --

$0
=====================

<<< Write introduction to recipes here>>>


How do we do it...
------------------
  <ul>

  <li> Create new project *$1* with *simple* stack template.

~~~
  stack new $1 simple
~~~

  <li> Add dependency on xxxxx library in the *build-depends* sub-section of *executable* section.

~~~
  executable $1
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
                       , xxxx
~~~

  <li> Open *src/Main.hs*. We will be adding our source here. 

  <li>

  <li> Build and execute the project.

~~~
  stack build
  stack exec -- $1
~~~

  You should see following output,

  </ul>


How did we do it...
-------------------


