module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

true = True
false = False

subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = map (x:) (subseqs xs) ++ subseqs xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = z
    where (_,z) = fibDuo (x)

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

myMap :: (a->b->c) -> [a] -> [b] -> [c]
myMap fun ali bli = [fun x y | (x,y) <- zip ali bli]

waitThreads fim = 
  do f <- takeMVar fim
     if (f > 0) then
         do putMVar fim f
            waitThreads fim
     else 
         return ()
         
type Semaphore = TVar Bool

p :: Semaphore -> STM()
p sem = do
    b <- readTVar sem
    if (b)
    then writeTVar sem false
    else retry

v :: Semaphore -> STM()
v sem = writeTVar sem true

ver :: Int -> Semaphore -> MVar Int -> IO()
ver i sem fim = do 
    atomically (v sem)
    if (i > 0)
    then ver (i-1) sem fim
    else do {f <- takeMVar fim;
         putMVar fim (f-1)}

per :: Int -> Semaphore -> MVar Int -> IO()
per i sem fim = do
    atomically (p sem)
    putStrLn(show i)
    if (i > 0)
    then per (i-1) sem fim
    else do {f <- takeMVar fim;
         putMVar fim (f-1)}

type Conta = TVar Int

saque :: Conta -> Int -> STM()
saque con val = do
    i <- readTVar con
    writeTVar con (i - val)

deposito :: Conta -> Int -> STM()
deposito con val = saque con (-val)

saque2 :: Conta -> Int -> STM()
saque2 con val = do
    i <- readTVar con
    if (0 < i)
    then writeTVar con (i - val)
    else retry

saque3 :: Conta -> Conta -> Int -> STM()
saque3 con1 con2 val = orElse (saque2 con1 val) (saque con2 val)

fornecedor :: TVar Int -> TVar Int -> TVar Int -> MVar Int -> IO()
fornecedor pao carne tomate fim = do
    atomically  (do
                    writeTVar pao 30
                    writeTVar carne 30
                    writeTVar tomate 30
                )
    f <- takeMVar fim
    putStrLn (show f)
    putMVar fim (f-1)
    fornecedor pao carne tomate fim

noNegatives :: [Int] -> Bool
noNegatives = foldr (\i b -> b && not(i<0)) True

produtor :: TVar Int -> TVar Int -> TVar Int -> MVar a -> MVar Int -> IO()
produtor pao carne tomate faca fim = do
    k <- takeMVar faca
    atomically  (do
                    p <- readTVar pao
                    c <- readTVar carne
                    t <- readTVar tomate
                    writeTVar pao (p-1)
                    writeTVar carne (c-1)
                    writeTVar tomate (t-1)
                )
    putMVar faca k
    putStrLn("sanduba preparado")
    f <- takeMVar fim
    putMVar fim (f-1)
    produtor pao carne tomate faca fim
    

main :: IO()
main = do
    pao <- atomically $ newTVar 0
    carne <- atomically $ newTVar 0
    tomate <- atomically $ newTVar 0
    faca <- newMVar true
    fim <- newMVar 1000
    forkIO(fornecedor pao carne tomate fim)
    forkIO(produtor pao carne tomate faca fim)
    forkIO(produtor pao carne tomate faca fim)
    waitThreads fim
    return()