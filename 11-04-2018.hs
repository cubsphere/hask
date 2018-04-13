true = True
false = False   

mdc :: Integer -> Integer -> Integer
mdc 0 y = y
mdc x y = mdc (mod y x) x

--it makes no sense to use the sieve of eratosthenes for this...
isPrime :: Integer -> Bool
isPrime 0 = false
isPrime 1 = false
isPrime 2 = true
isPrime x = and [mdc x y == 1 | y <- [2..ceiling(sqrt(fromIntegral x))]]
--could use something faster than mdc, but i'm too lazy to figure out the proper types and all

type Point = (Double,Double,Double)
distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt(xd*xd + yd*yd + zd*zd)
    where xd = x2 - x1
          yd = y2 - y1
          zd = z2 - z1 

squaresSum = sum [x*x | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid x y = [ (i,j) | i <- [0..x], j <- [0..y] ]

square :: Int -> [(Int, Int)]
square x = [ (i,j) | i <- [0..x], j <- [0..x], i/=j ]

merge :: Ord a => [a] -> [a] -> [a]
merge [] li = li
merge li [] = li
merge (x:xs) (y:ys) |x<y = x:(merge xs (y:ys))
                    |otherwise = y:(merge (x:xs) ys)

halve :: [a] -> ([a], [a])
halve li = (take half li, drop half li)
    where half = floor(fromIntegral(length li)/2)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort li = merge (mergesort h1) (mergesort h2)
    where (h1, h2) = halve li

listMap :: [a->b] -> [a] -> [[b]]
listMap fli li = [map f li | f <- fli]

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Ord, Show, Enum)

--you can order them by mergesort

funnyFilter :: Eq a => [(a,b)] -> a -> [b]
funnyFilter li k = [ y | (x,y) <- li, x == k]