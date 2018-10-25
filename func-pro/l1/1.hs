-------------------------------------------------------------------------
--  
--         Tree.hs
--  
--     Search trees as an ADT                   
--                                  
--         (c) Addison-Wesley, 1996-2011.                   
--  
-------------------------------------------------------------------------

import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Tree

instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
    arbitrary = sized arbTree
arbTree 0 = return nil
arbTree n = frequency [ (1, (arbTree 0)), (9, node <$> arbitrary <*> (arbTree (n `div` 2)) <*> (arbTree (n `div` 2)))]