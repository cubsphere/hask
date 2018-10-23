import Test.QuickCheck  
import Control.Monad
import Data.List


 {-

instance  (Arbitrary , Arbitrary b) => Arbitrary  a b 
  where arbitrary = liftM  (,)  arbitrary arbitrary


--}
{-
main = 
   do
     sample ( oneof [return True, return False])
     sample (sized $ (\n -> choose (0, n)) )
-}
-- > sample (choose (0,length [1..20]-1))
-- > sample ( oneof [return True, return False])

dado :: Gen Int
dado = choose (1, 6)
-- generate dado

arbitraryBool :: Gen Bool
arbitraryBool = choose (True, False)

{-
instance Arbitrary Int where
    arbitrary = choose (-20, 20)
-}
 

{-  
instance Arbitray (Arbitrary a, Arbitrary b) =>
   Arbitrary (a, b) where
     arbitrary = liftM2 (,) arbitrary arbitrary


instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
    arbitrary = do x <- arbitrary
                   y <- arbitrary 
                   return (x,y)

instance Arbitrary a => Arbitrary [a] where     
  arbitrary = oneof [return [], liftM2 (:)  arbitrary arbitrary]
-}

data Colour = Red| Blue | Green
   deriving (Show)

instance Test.QuickCheck.Arbitrary Colour where
    arbitrary = oneof [return Red, return Blue, return Green]

----------------------------------------------------

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

{-  
instance Arbitrary Ternary where
    arbitrary     = elements [Yes, No, Unknown]
-}

--{-
instance Arbitrary Ternary where
        arbitrary     = do
            n <- choose (0, 2) :: Gen Int
            return $ case n of
                          0 -> Yes
                          1 -> No
                          _ -> Unknown

--}

{-
data Tree = Leaf Int | Branch Tree Tree 
            deriving (Show)

tree = oneof [liftM Leaf arbitrary, liftM2 Branch tree tree]


tree1 = sized tree'
tree' 0 = liftM Leaf arbitrary
tree' n = -- | n>0 = 
   oneof [liftM Leaf arbitrary,
         liftM2 Branch subtree subtree]
   where subtree = tree' (n `div` 2)
--}

data Tree a = Leaf a | Branch (Tree a) (Tree a)
              deriving (Show)

{-
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency 
      [ (1, liftM Leaf arbitrary), 
      (2, liftM2 Branch arbitrary arbitrary) ]
--}

--{-
instance Arbitrary a => Arbitrary (Tree a) where
        arbitrary = sized arbTree        
arbTree 0 = liftM Leaf arbitrary
arbTree n = frequency [ (1, liftM Leaf arbitrary)
  , (4, liftM2 Branch (arbTree (n `div` 2)) (arbTree (n `div` 3)))]


--}

data Doc = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show,Eq)

--instance Arbitrary Char where
--    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")

{-
instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)
-}
--{-
instance Test.QuickCheck.Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]
--}

empty :: Doc
(<>)  :: Doc -> Doc -> Doc

empty = Empty

Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

--}


{-
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a,b,c) where
    arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
-}

--isOrdered :: [Integer] -> Bool
isOrdered (x:y:zs) = x <= y && isOrdered (y:zs)
isOrdered _ = True 

-- Gerador de listas

--{-

genList1 ::  (Arbitrary a) => Gen [a]
genList1 = liftM2 (:) arbitrary genList1

genList2 ::  (Arbitrary a) => Gen [a]
genList2 = oneof [ return []
                 , liftM2 (:) arbitrary genList2]

genList3 ::  (Arbitrary a) => Gen [a]
genList3 = frequency [ (1, return [])
                      , (7, liftM2 (:) arbitrary genList3) ]

genOrdList = genList3 >>= return . sort

-- quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }
-- > quickCheckN 1000 $ forAll genOrdList isOrdered 

prop_insert :: Int -> Property 
prop_insert x = forAll genOrdList $ \xs -> isOrdered xs && isOrdered (insert x xs)

prop_insert2 :: Int -> Property 
prop_insert2 x = forAll genOrdList $ \xs ->
  isOrdered xs && isOrdered (insert x xs)

--}

{-
newtype OrdList a = OrdList [a] deriving (Eq, Ord, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (OrdList a) where
   arbitrary = liftM OrdList genOrdList


-- ghci> quickCheckN 1000 prop_insert 
-}