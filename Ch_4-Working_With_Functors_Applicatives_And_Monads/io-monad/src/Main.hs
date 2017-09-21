module Main where

import System.IO (hGetLine, hIsEOF, withFile, Handle, IOMode(..))
import System.Environment (getArgs)
import Control.Monad
import Data.List (intercalate)

-- From the file handle, check if we have reached end of file,
-- otherwise read the file line by line
getLinesSeq :: Handle -> IO [String]
getLinesSeq h = do
  eof <- hIsEOF h
  -- Use (:) to get the line and append remaining ...
  if eof then return [] else (:) <$> hGetLine h <*> getLinesSeq h

-- Print line number and string separated by :
printLine :: (Int, String) -> IO ()
printLine (lineno, line) = putStrLn $ intercalate " : " [show lineno, line]

-- Given a monad that gives us list of strings, return the list of
-- (int,string) where int is the line number, and string represents
-- the corresponding line.
withLineNumbers :: Monad m => m [String] -> m [(Int,String)]
withLineNumbers m = zip <$> pure [1..] <*> m
    
main :: IO ()
main = do
  -- Throw an error if number of arguments is not 1
  args <- getArgs
  when (length args /= 1) $ do
    putStrLn $ "Incorrect arguments " ++ (show args)
    error "Provide file name"

  -- Open the file.
  withFile (head args) ReadMode (\h -> do
                                    -- Each line is zipped with line number
                                    lines <- withLineNumbers (getLinesSeq h)
                                    -- Print line
                                    forM_ lines printLine
                                )
  
  
