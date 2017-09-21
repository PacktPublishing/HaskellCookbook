module Main where

import Data.Maybe

main :: IO ()
main = do
  putStrLn "Using Maybe"
  let i = Just 10 :: Maybe Int
      j = Just 2 :: Maybe Int
      z = Just 0 :: Maybe Int
  putStrLn $ "Does (Just 10) represent a value? " ++ (show $ isJust i)
  putStrLn $ "Does (Nothing) represent a value? " ++ (show $ isJust Nothing)
  putStrLn $ "Does (Nothing) is really Nothing? " ++ (show $ isNothing Nothing)

  putStrLn ""
  putStrLn $ "Singleton List and Maybe interoperability"
  putStrLn $ "Converting list [10] to Maybe : " ++ (show $ listToMaybe [10])
  putStrLn $ "Converting empty list to Maybe (Nothing) : " ++ (show $ (listToMaybe [] :: Maybe Int))

  putStrLn $ "Converting Maybe (Just 10) to list : " ++ (show $ maybeToList (Just 10))
  putStrLn $ "Converting Maybe (Nothing) to list : " ++ (show $ maybeToList (Nothing :: Maybe Int))

  putStrLn ""
  putStrLn "Using default value for a transforamtion using 'maybe'"
  putStrLn $ "Use NULL if Nothing, and convert a value to string if Maybe holds a value"
  let defaultNull = "NULL"
      convertToString = maybe defaultNull show
      null = convertToString Nothing
      something = convertToString (Just 10)
  putStrLn $ "Converting Nothing to String : " ++ null
  putStrLn $ "Converting a value to String : " ++ something

  putStrLn ""
  putStrLn $ "Getting value from (Just 10) = " ++ (show $ fromJust i)
  putStrLn $ "Safe Division - 10 / 2"
  let safeAnswer1 = safeDiv i j
  putStrLn $ "Answer is " ++ (show safeAnswer1)
  putStrLn ""
  putStrLn $ "Safe Division by Zero - 10/0"
  let safeAnswer2 = safeDiv i z
  putStrLn $ "Answer is " ++ (show safeAnswer2)

  putStrLn ""
  putStrLn $ "We can also use - do notation"
  let safeAnswer3 = safeDiv1 i z
  putStrLn $ "Safe Division by Zero using do notation - 10 / 0 = " ++ (show safeAnswer3)
  
  putStrLn ""
  let evens = mapMaybe (\x -> if odd x then Nothing else (Just x)) [1..10]
  putStrLn $ "Filtering out odd elements - mapMaybe (\\a -> if odd a then Nothing else (Just a)) [1..10] = " ++ (show evens)
  

  
safeOperation :: Num a => (a -> a -> Bool) -> (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
-- If any of the input is Nothing, then the output is Nothing
safeOperation _  _   Nothing  _         = Nothing
safeOperation _  _   _        Nothing   = Nothing
safeOperation c  _   (Just i) (Just j)  | c i j = Nothing
safeOperation c  op  (Just i) (Just j)  = Just (i `op` j)

safeDiv :: Maybe Int -> Maybe Int -> Maybe Int
safeDiv = safeOperation divCondition div
  where
    divCondition _ 0 = True
    divCondition _ _ = False

safeDiv1 :: Maybe Int -> Maybe Int -> Maybe Int
safeDiv1 i j = do
  xi <- i
  xj <- j
  if 0 == xj
    then
    Nothing
    else
    return (xi `div` xj)

