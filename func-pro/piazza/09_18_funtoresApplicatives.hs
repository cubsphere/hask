{- 
instance Functor Lista where
  -- fmap :: (a -> b) -> f a -> f b
    fmap g [] = []
    fmap g (x:xs) = fmap g xs ++ [g x]
     -}

data Lista a = Vazia | Cons a (Lista a) deriving Show

{- instance Functor Lista where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g Vazia = Vazia
    fmap g (Cons x l) = Cons (g x) (fmap g l)

 -}
 
 {-
instance Functor Lista where
        -- fmap :: (a -> b) -> f a -> f b
        fmap g Vazia = Vazia
        fmap g (Cons x l) = Cons (fmap g l) (g x) 
-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure ( : ) <*> getChar <*> getChars (n-1)
    
