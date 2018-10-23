import Data.List
import Test.QuickCheck


mNot :: Bool -> Bool
mNot True = False
mNot False = True

prop_mNot :: Bool -> Bool
prop_mNot x =  
    not x == mNot x


exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

exOr1 :: Bool -> Bool -> Bool
exOr1 True x = not x
exOr1 False x = x

prop_exOrs :: Bool -> Bool -> Bool
prop_exOrs x y =
    exOr x y == exOr1 x y

prop_exOrs2 :: Bool -> Bool -> Bool
prop_exOrs2 x y =
    exOr x y == (x /= y)        



maxi :: Integer -> Integer -> Integer
maxi x y 
 | x >= y = x
 | otherwise = y

max' :: Integer -> Integer -> Integer
max' x y =
   if x >= y then x else y

prop_compareMax :: Integer -> Integer -> Bool
prop_compareMax x y = 
    maxi x y == max' x y 


prop_max1, prop_max2 :: Integer -> Integer -> Bool

prop_max1 x y =
  x <= max x y && y <= max x y

prop_max2 x y =
  x <= max x y || y <= max x y

prop_max3 :: Integer -> Integer -> Bool
prop_max3 x y = 
     (x == maxi x y) `exOr` (y == max x y)


--reverse [x] = [x]
--reverse (reverse xs) == xs
--reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_RevUnit :: Int -> Bool
prop_RevUnit x = 
    reverse [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool    
prop_RevApp xs ys = 
    reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_RevRev ::  [Int] -> Bool
prop_RevRev xs =
    reverse(reverse xs) == xs
    
prop_RevApp2 :: [Int] -> [Int] -> Bool
prop_RevApp2 xs ys =
    reverse (xs ++ ys) == reverse xs ++ reverse ys   

--- Leis condicionais

prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y  =  x <= y ==> max x y == x

--prop_Insert :: Int −> [Int] −> Property
--prop_Insert x xs = ordered xs ==> ordered (insert x xs)
    

-- Quicksort

qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs  = [y | y <- xs, y < x] 
        rhs  = [z | z <- xs, z > x]

{-

isOrdered :: Ord a => [a] -> Bool
isOrdered = undefined

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = undefined
-}

--isOrdered :: [Integer] -> Bool
isOrdered (x:y:zs) = x <= y && isOrdered (y:zs)
isOrdered _ = True 

prop_qsort_idemp ::  [Int] -> Bool 
prop_qsort_idemp xs = qsort (qsort xs) == qsort xs

prop_qsort_min :: [Int] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum xs

prop_qsort_nn_min    :: [Int] -> Property
prop_qsort_nn_min xs = 
  not (null xs) ==> head (qsort xs) == minimum xs

prop_qsort_nn_max    :: [Int] -> Property
prop_qsort_nn_max xs = 
  not (null xs) ==> last (qsort xs) == maximum xs

prop_qsort_sort    :: [Int] -> Bool
prop_qsort_sort xs =  qsort xs == sort xs


isDistinct :: (Eq a) => [a] -> Bool
isDistinct []     = True
isDistinct (x:xs) = x `notElem` xs && isDistinct xs

prop_qsort_distinct :: [Int] -> Bool 
prop_qsort_distinct = isDistinct . qsort 

prop_qsort_distinct_sort :: [Int] -> Property
prop_qsort_distinct_sort xs = 
  isDistinct xs ==> qsort xs == sort xs




minsert x []                 = [x]
minsert x (y:ys) | x > y     = x : y : ys
                  | otherwise = y : minsert x ys

isort :: Ord a => [a] -> [a]
isort = foldr insert []

prop_isort_sort    :: [Int] -> Bool
prop_isort_sort xs = isort xs == sort xs

prop_insert_ordered      :: Int -> [Int] -> Property 
prop_insert_ordered x xs = 
  isOrdered xs ==> isOrdered (insert x xs)

--- 

prop_Insert :: Int -> [Int] -> Property
prop_Insert  x xs =
  isOrdered xs ==> isOrdered (insert x xs)


prop_Insert2 :: Int -> [Int] -> Property
prop_Insert2  x xs =
  isOrdered xs ==> 
   classify (null xs) "trivial" $
     isOrdered (insert x xs)

prop_Insert3 :: Int -> [Int] -> Property
prop_Insert3  x xs =
       isOrdered xs ==> 
        collect (length xs)  $
          isOrdered (insert x xs)

prop_Insert4 :: Int -> [Int] -> Property
prop_Insert4  x xs =
   forAll orderedList $ \xs ->
     isOrdered (insert x xs)


prop_insert_ordered_vacuous' :: Int -> [Int] -> Property 
prop_insert_ordered_vacuous' x xs = 
 collect (length xs) $
  classify (isOrdered xs) "ord" $
   classify (not (isOrdered xs)) "not-ord" $
    not (isOrdered xs) || isOrdered (insert x xs)

    
prop_Insert_obs x xs = 
       isOrdered xs ==> 
        collect (length xs)$
        classify (isOrdered (x:xs)) "at-head"$
         classify (isOrdered (xs++[x])) "at-tail"$
         isOrdered (insert x xs)
  where types = x::Int
    


--------------------
prop_DoubleCycle :: [Int] -> Property
prop_DoubleCycle xs =
   not (null xs) ==> cycle xs == cycle (xs ++ xs)

prop_DoubleCycle2 :: [Int] -> Int -> Property
prop_DoubleCycle2 xs n = 
 not (null xs) && n >= 0 ==> 
   take n (cycle xs ) == take n (cycle (xs ++ xs))

-- Geracao de dados de teste

