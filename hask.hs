t = True
f = False
true = t
false = f

subsetSet :: [t] -> [[t]]
subsetSet [] = [[]]
subsetSet (x:xs) = map (x:) (subsetSet xs) ++ subsetSet xs 

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