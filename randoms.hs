true = True
false = False

subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = map (x:) (subseqs xs) ++ subseqs xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = z
    where (y,z) = fibDuo (x)

fibDuo :: Int -> (Int, Int)
fibDuo 1 = (0,1)
fibDuo x = (y, z)
    where (y, z) = (fb2, fb1+fb2)
          (fb1, fb2) = fibDuo (x-1)

test1 :: Int -> Int -> Int
test1 _ _ = 0

test2 :: Int -> Bool
test2 _ = false

test3 :: Int -> Int
test3 _ = 0