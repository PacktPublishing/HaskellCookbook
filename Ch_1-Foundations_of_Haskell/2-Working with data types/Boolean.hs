module Boolean where


import Prelude hiding (True, False)


data Boolean a = Boolean a -- Lifting a value to Boolean
               | And (Boolean a) (Boolean a) 
               | Or (Boolean a) (Boolean a)
               | Not (Boolean a)
               | True
               | False
               deriving Show


