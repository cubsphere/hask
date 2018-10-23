module Main where

import Control.Concurrent
import Control.Concurrent.MVar

waitThreads fim = 
  do f <- takeMVar fim
     if (f > 0) then
         do putMVar fim f
            waitThreads fim
     else 
         return ()

bulboProducer :: MVar Int -> MVar Int -> IO()
bulboProducer var fim = do
    i <- takeMVar var
    putMVar var (i+1)
    putStrLn((show (i+1)) ++ " bulbos")
    f <- takeMVar fim
    putMVar fim (f-1)
    bulboProducer var fim

soqueteProducer :: MVar Int -> MVar Int -> IO()
soqueteProducer var fim = do
    i <- takeMVar var
    putMVar var (i+1)
    putStrLn((show (i+1)) ++ " soquetes")
    f <- takeMVar fim
    putMVar fim (f-1)
    soqueteProducer var fim

embalagemProducer :: MVar Int -> MVar Int -> IO()
embalagemProducer var fim = do
    i <- takeMVar var
    putMVar var (i+1)
    putStrLn((show (i+1)) ++ " embalagens")
    f <- takeMVar fim
    putMVar fim (f-1)
    embalagemProducer var fim

montador :: MVar Int -> MVar Int -> MVar Int -> MVar Int -> MVar Int -> IO()
montador bulbos soquetes embalagens caixa fim = do
    intBulbos <- takeMVar bulbos
    intSoquetes <- takeMVar soquetes
    intEmbalagens <- takeMVar embalagens
    intCaixa <- takeMVar caixa
    putMVar bulbos (intBulbos - 1)
    putMVar soquetes (intSoquetes - 1)
    putMVar embalagens (intEmbalagens - 1)
    putMVar caixa (intCaixa + 1)
    putStrLn("lampada produzida")
    f <- takeMVar fim
    putMVar fim (f-1)
    montador bulbos soquetes embalagens caixa fim

transportador :: MVar Int -> MVar Int -> IO()
transportador caixa fim = do
    intCaixa <- takeMVar caixa
    if (intCaixa == 50)
    then do {putMVar caixa 0;
         putStrLn("caixa transportada")}
    else do {putMVar caixa intCaixa;
         putStrLn("caixa não transportada")}    
    f <- takeMVar fim
    putMVar fim (f-1)
    transportador caixa fim

main :: IO()
main =  do
    bulbos <- newMVar(0)
    soquetes <- newMVar(0)
    embalagens <- newMVar(0)
    caixa <- newMVar(0)
    fim <- newMVar(100000)
    forkIO(bulboProducer bulbos fim)
    forkIO(soqueteProducer soquetes fim)
    forkIO(embalagemProducer embalagens fim)
    forkIO(montador bulbos soquetes embalagens caixa fim)
    forkIO(transportador caixa fim)
    waitThreads fim
    putStrLn("done")    