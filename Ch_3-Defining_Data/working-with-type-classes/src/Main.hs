module Main where

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
           deriving Show


instance Enum Month where
  toEnum 0 = January
  toEnum 1 = February
  toEnum 2 = March
  toEnum 3 = April
  toEnum 4 = May
  toEnum 5 = June
  toEnum 6 = July
  toEnum 7 = August
  toEnum 8 = September
  toEnum 9 = October
  toEnum 10 = November
  toEnum 11 = December
  toEnum n = toEnum $ n `rem` 12

  fromEnum January = 0
  fromEnum February = 1
  fromEnum March = 2
  fromEnum April = 3
  fromEnum May = 4
  fromEnum June = 5
  fromEnum July = 6
  fromEnum August = 7
  fromEnum September = 8
  fromEnum October = 9
  fromEnum November = 10
  fromEnum December = 11

instance Eq Month where
  m1 == m2 = fromEnum m1 == fromEnum m2


instance Ord Month where
  m1 `compare` m2 = fromEnum m1 `compare` fromEnum m2

data RoseTree a = RoseTree a [RoseTree a]

toString :: Show a => RoseTree a -> String -> String
toString (RoseTree a branches) =
  ( "<<" ++) . shows a . ('[':) . branchesToString branches . (']':) .  ( ">>" ++)
  where
    branchesToString [] r = r
    branchesToString (x:[]) r = branchesToString [] (toString x "" ++ r)
    branchesToString (x:xs) r = branchesToString xs (',' : toString x "" ++ r)

instance Show a => Show (RoseTree a) where
  show tree = toString tree ""

instance Read a => Read (RoseTree a) where
  readsPrec prec ('<':'<':s) =
    case readsPrec prec s of
      [(a,t)] -> case readList t of
                   [(as,ts)] -> case ts of
                                  ('>':'>':ss) -> [(RoseTree a as, ss)]
                                  _ -> []
                   _ -> []
      _ -> []
  readsPrec prec _ = []

  readList xs = let readList' ('[':ys) rs = case readsPrec 0 ys of
                                              [(r,zs)] -> readList' zs (r:rs)
                                              _ -> readList' ys rs
                                              
                    readList' (',':ys) rs = case readsPrec 0 ys of
                                              [(r,zs)] -> readList' zs (r:rs)
                                              _ -> []

                    readList' (']':ys) rs = [(rs,ys)]
                    readList' _  _ = []
 
                in readList' xs []

main :: IO ()
main = do
  putStrLn "Enumerating months"
  putStrLn $ show [January ..December]
  putStrLn "Enumerating odd months"
  putStrLn $ show [January,March .. December]
  putStrLn $ "Equating months, January with itself : "
    ++ (show $ January == January)
    ++ " and January with February : "
    ++ (show $ January == February)
  putStrLn $ "Using /= function"
  putStrLn $ "Not equating months, January with itself : "
    ++ (show $ January /= January)
    ++ " and January with February : "
    ++ (show $ January /= February)
  putStrLn $ "Comparing months, January with itself : "
    ++ (show $ January `compare` January)
    ++ " and January with February : "
    ++ (show $ January `compare` February)

  putStrLn ""
  putStrLn "Creating a tree"

  let singleton = RoseTree 10 []
      tree = RoseTree 10 [RoseTree 13 [RoseTree 11 []], RoseTree 7 [], RoseTree 5 [RoseTree 3 []]]

  putStrLn ""
  putStrLn $ "Showing singleton tree : " ++ show singleton
  putStrLn $ "Showing tree : " ++ show tree

  putStrLn ""
  putStrLn $ "Read what you show -- show (read (show tree) )"
  putStrLn $ "Singleton Tree - " ++ show (read (show singleton) :: RoseTree Int)
  putStrLn $ "Tree - " ++ show (read (show tree) :: RoseTree Int)
  
  
