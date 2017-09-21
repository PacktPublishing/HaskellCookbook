In this chapter, we will be looking at following recipes,

* Working with Getter and Setter
* Define Lens for user types
* Working with Lenses
* Understand Prism
* Relation between Traversable and Prism
* Working with Prisms
* Working with predefined lenses

Introduction
===============

It is said that *Lens* and *Prism* are the one of the most complex piece of code written in Haskell, thanks to use of existentials and many *operators* that can work with each other. Thankfully, working with *Lens* and *Prism* is not that hard, and definitely very productive due to its usefulness.

We will be using Edward Kmett's original *lens* library for this chapter.

