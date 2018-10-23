{-
instance Functor [] where
  --fmap :: (a -> b) -> [a] -> [b]
  fmap = map
-}

data MMaybe a = MNothing | MJust a deriving Show

instance Functor MMaybe where
  --fmap :: (a -> b) -> Maybe a  -> Maybe b
  fmap _ MNothing = MNothing
  fmap f (MJust x) = MJust (f x)




data Tree a = Leaf a | Node (Tree a) (Tree a) 
                    deriving Show

instance Functor Tree where
 -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

