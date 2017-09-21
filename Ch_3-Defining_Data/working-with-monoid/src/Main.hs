module Main where

import Data.Monoid

data Option = Option { boolOption :: Bool, selections :: [String] }
            deriving Show

instance Monoid Option where

  mempty = Option False []

  (Option b1 s1) `mappend` (Option b2 s2) = Option (b1 || b2) (s1 ++ s2)

main :: IO ()
main = do
  putStrLn "Define default options"
  let defaultOptions = mempty :: Option
  putStrLn (show defaultOptions)
  let option1 = defaultOptions `mappend` (Option True [])
      option2 = option1 `mappend` (Option False ["haskell"])
      option3 = option2 `mappend` (Option True ["cookbook"])

  putStrLn $ "Adding True flag - " ++ show option1
  putStrLn $ "Adding False flag, and selection \"haskell\" - "
    ++ show option2
  putStrLn $ "Adding True flag, and selection \"cookbook\" - "
    ++ show option3

  putStrLn $ "Contatenating all options"
  putStrLn $ "Concatenation Result - " ++ show (mconcat [defaultOptions, option1, option2 ])
