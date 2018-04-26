--Lucas Miranda Lin
import Data.List

--1
s1 = (+2)
s2 = (\x -> x > 3)

--2
data CInt = Conjunto [Int] deriving (Show)

--2a
removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:y:xs) | x==y = removeDuplicates (x:xs)
| otherwise = x:(removeDuplicates (y:xs))
makeSet :: [Int] -> CInt
makeSet = Conjunto . removeDuplicates . Data.List.sort

--2b
union :: CInt -> CInt -> CInt
union (Conjunto x) (Conjunto y) = makeSet (x ++ y)

--2c
mapSet :: (Int -> Int) -> CInt -> CInt
mapSet fun (Conjunto x) = Conjunto $ map fun x

--3
type Texto = String
type Id = String
type DataHoraPub = Int
data Post = Post (Id, DataHoraPub) Texto deriving (Show, Eq)
data Thread = Nil | T Post (Thread)

--3a
instance Show Thread where
show Nil = ""
show (T (Post (id, dhp) tex) thr) = "(" ++ id ++ " " ++ (Prelude.show dhp) ++ " " ++ tex ++ ")"
++ (Main.show thr)

--3b
inserirPost :: Post -> Thread -> Thread
inserirPost p t = T p t

--3c
threadToList :: Thread -> [Post]
threadToList Nil = []
threadToList (T p t) = p:(threadToList t)

--3d
listToThread :: [Post] -> Thread
listToThread [] = Nil
listToThread (x:xs) = T x $ listToThread xs

--3e
removeFilter :: (Id, DataHoraPub) -> Post -> Bool
removeFilter (id1, dhp1) (Post (id2, dhp2) _) = (id1 /= id2 || dhp1 /= dhp2)
removerPost :: (Id, DataHoraPub) -> Thread -> Thread
removerPost pair = listToThread . (filter $ removeFilter pair) . threadToList