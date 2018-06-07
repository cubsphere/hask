module Main where
import Control.Concurrent
import Control.Concurrent.MVar

threadA::MVar Float -> MVar Float -> IO()
threadA s r
     = do putMVar s 5
          z <- takeMVar r
          putStrLn (Prelude.show z)

threadB::MVar Float -> MVar Float -> IO()
threadB r s
     = do z <- takeMVar r
          putMVar s (z*500)

ab :: IO ()
ab = do aMVar <- newEmptyMVar
        bMVar <- newEmptyMVar
        forkIO (threadA aMVar bMVar)
        forkIO (threadB aMVar bMVar)
        threadDelay 2000
        return()

data Counter = Counter (MVar Int)

instance Show Counter where
show (Counter x) = Prelude.show x

decrement :: Counter -> IO()
decrement (Counter num) =
    do z <- takeMVar num
       putMVar num (z-1)

increment :: Counter -> IO()
increment (Counter num) =
    do z <- takeMVar num
       putMVar num (z+1)

main::IO()
main = do c <- (Counter 0)
          increment c
          increment c
          putStrLn (Main.show c)
          return()