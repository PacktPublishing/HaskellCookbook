# Working with Map

In this recipe, we will be looking at Data.Map. A map keeps an association between key and the corresponding value. The map stores the ordered key and their values (dictionaries). There are two variants in the container library, strict and lazy. We will be looking at strict variant. The lazy variant has same interface, except that the implementation is lazy.


## How do we do it...

* Create a new project work-with-map using *simple* stack template. 
* Add containers library to the build-depends subsection of the executable subsection. 
  ```
    executable working-with-map
      hs-source-dirs:      src
      main-is:             Main.hs
      default-language:    Haskell2010
      build-depends:       base >= 4.7 && < 5
                         , containers
  ```
* Open src/Main.hs, we will be using this as our playground for dealing with map.





