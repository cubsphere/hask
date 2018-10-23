true = True
false = False

testListRecursion :: (a->Bool) -> [a] -> Bool
testListRecursion _ [] = True
testListRecursion f (x:xs) |(f x) = testListRecursion f xs
                           |otherwise = false

testListMapAnd :: (a->Bool) -> [a] -> Bool
testListMapAnd f x = and (map f x)

testListFoldr :: (a->Bool) -> [a] -> Bool
testListFoldr f x = foldr (\curr -> (\accu -> f curr && accu)) true x

factorialListComprehension :: Int -> [Int]
factorialListComprehension x = [product [1..y] | y <- [1..x]]

factorialList :: Int -> [Int] -- a lot more efficient than the version above
factorialList 1 = [1]
factorialList x = prev ++ [(last prev) * x]
    where prev = factorialList (x-1)

type Name = String
type Content = String

data File = SimpleFile Name Content | Directory Name [File]

instance Show File where
    show (SimpleFile n c) = n
    show (Directory n li) = n

--couldn't get past here...