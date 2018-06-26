--Lucas Miranda lin - lml
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

type Buffer a = TVar [a]

newBuffer :: a -> IO (Buffer a)
newBuffer x = newTVarIO [x]

put :: Buffer a -> a -> STM()
put buffer elem = do
    buf <- readTVar buffer
    writeTVar buffer (buf ++ [elem])
 
isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty _ = True

get :: Buffer a -> STM a
get buffer = do
    tempbuf <- readTVar buffer
    check(isNotEmpty tempbuf)
    (x:buf) <- readTVar buffer
    writeTVar buffer buf;
    return x