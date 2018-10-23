{-
mfoldr1 :: (a -> a -> a) -> [a] -> a
mfoldr1 f [v] = v
mfoldr1 f (x:xs) = f x (mfoldr1 f xs)

somaLista l = mfoldr1 (+) l

mfoldl :: (a -> b -> a)  -> a -> [b] -> a
mfoldl f v [] = v
mfoldl f v (x:xs) = mfoldl f (f v x) (xs)


-- recursao mutua

par :: Int -> Bool
par 0 = True
par n = impar (n-1)

impar :: Int -> Bool
impar 0 = False
impar n = par (n-1)

somaquadpares l = 
 sum ( map (^2)  (filter (\x -> x `mod` 2 ==0 ) l ))


--- 


type Pos = (Int, Int)
type Trans = Pos -> Pos

-- type TreeM = (Int, [TreeM]) 

type Par a = (a, a)
type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data MBool = MTrue | MFalse

data Move = Norte | Sul | Leste | Oeste

move :: Move -> Pos -> Pos
move Norte (x,y) = (x , y+1)
move Sul (x,y) = (x, y -1)
move Leste (x, y) = (x + 1, y)
move Oeste (x, y) = (x - 1, y) 

--

data MaybeM a = Nada | Apenas a

safediv :: Int -> Int -> MaybeM Int
safediv _ 0 = Nada
safediv m n = Apenas (m `div` n)

data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int (n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

adicao :: Nat -> Nat -> Nat
adicao m n = int2nat (nat2int m + nat2int n)

adicao2 :: Nat -> Nat -> Nat
adicao2 Zero n = n
adicao2 (Succ m) n = Succ (adicao2 m n)

data Lista a = Nulo | Const a (Lista a)

len :: Lista a -> Int
len Nulo = 0
len (Const _ xs ) = 1 + len xs 

data Arvore a = Folha a | No (Arvore a ) a (Arvore a)

ocorre ::  Eq a  => a -> Arvore a -> Bool
ocorre x (Folha n) = x == n
ocorre x (No l y r)  = x == y || ocorre x l || ocorre x r 

{-
class Eq a where
    (==), (\=) :: a -> a -> Bool
    x /= y = not (x == y)
-}

{-
instance Eq Bool where
     True == True = True
     False == False = True
     _ == _ False

instance Ord Bool where
    False < True = True
    _ < _ = False
    b <= c = (b < c) || (b == c)
    b > c = c < b
    b > = c = c <= b

-}

data DiasUteis = Seg | Ter | Qua | Qui | Sex
          deriving (Show, Eq, Ord, Enum)

-}

--Entrada e Saida
{-
imprimirStr :: IO ()
imprimirStr = do putStrLn "asdf"
                 putStrLn "wert"

readLines :: IO()
readLines = do getLine
               getLine
               putStrLn "Valores lidos"

reverseLines :: IO()
reverseLines = do l1  <- getLine
                  l2  <- getLine
                  putStrLn (reverse l1)
                  putStrLn (reverse l2)
-}

main = do return ()
          l1  <- return "asdf"
          line <- getLine
          return "nada"
          return 4
          putStrLn line
          putStrLn l1


{-
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh

-}

{-
IOMode: ReadMode, WriteMode, ReadWriteMode, AppendMode                  
-}

