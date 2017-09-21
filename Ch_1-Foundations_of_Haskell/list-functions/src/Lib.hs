module Lib where

-- 1 List creation
emptyList = []

-- 2 Prepend an item to the list
prepend = 10 : []

-- 3 Create a list of 5 integers
list5 = 1 : 2 : 3 : 4 : 5 : []

-- 4 Create a list of integers from 1 to 10
list10 = [1..10]

-- 5 Create an infinite list
infiniteList = [1..]

-- 6 Head of a list
getHead = head [1..10]

-- 7 Tail of a list
getTail = tail [1..10]

-- 8 All but last
allbutlast = init [1..10]

-- 9 Take 10 elements
take10 = take 10 [1..]

-- 10 Drop 10 elements
drop10 = drop 10 [1..20]

-- 11 Get n'th element
get1331th = [1..] !! 1331

-- 12 Is element of the list
is10element = elem 10 [1..10]

-- 13 Pattern matching on list
isEmpty [] = True
isEmpty _ = False

-- 14 Pattern matching more elements
isSize2 (x:y:[]) = True
isSize2 _ = False

-- 15 Concatenate two lists
cat2 = [1..10] ++ [11..20]

-- 16 String as list
a2z = ['a'..'z']

-- 17 String as list
strHead = head "abc"

-- 18 Zip two lists
zip2 = zip ['a'..'z'] [1.. ]

