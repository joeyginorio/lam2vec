{- FinSet.hs
   =========
   Defines the category FinSet. -}

module FinSet where

import qualified Data.Set as Set
import Control.Monad (replicateM)

{- ================================= FinSet ================================= -}

-- The category FinSet has as objects finite sets, and as morphisms functions
-- between these sets represented as an exponential object.

-- Objects:   Inhabitants of type FinSet a
-- Morphisms: Exponential objects in FinSet a
type FinSet a = Set.Set a

-- FinSet is a cartesian category, meaning it's equipped with:
-- (i)   products         :: FinSet a -> FinSet b -> FinSet (a,b)
-- (ii)  left projection  :: FinSet (a,b) -> FinSet a
-- (iii) right projection :: FinSet (a,b) -> FinSet b
-- (iv)  terminal object  :: a -> FinSet a

prod :: FinSet a -> FinSet b -> FinSet (a,b)
prod = Set.cartesianProduct

lproj :: Ord a => FinSet (a,b) -> FinSet a
lproj = Set.map fst

rproj :: Ord b => FinSet (a,b) -> FinSet b
rproj = Set.map snd

terminal :: a -> FinSet a
terminal = Set.singleton

-- FinSet is also a cartesian closed category, meaning it's equipped with:
-- (v)  Exponentials   :: FinSet a -> FinSet b -> FinSet (Map a b)
-- (vi) Eval morphism  :: FinSet ((Map a b), a) -> FinSet b

type Map a b = [(a,b)]

expon :: (Ord a, Ord b) => FinSet a -> FinSet b -> FinSet (Map a b)
expon as bs = Set.fromList $ functions as' bs'
              where as' = Set.toList as
                    bs' = Set.toList bs

eval :: (Ord a, Ord b) => FinSet (Map a b, a) -> Maybe (FinSet b)
eval fxs = fmap Set.fromList ys
             where fs = Set.toList . Set.map fst $ fxs
                   xs = Set.toList . Set.map snd $ fxs
                   ys = sequence $ zipWith lookup xs fs

-- Generates all functions between two lists, using first list as domain
-- and second list as codomain.
-- e.g. functions [1,2] [3,4]
--      => [[(1,3),(2,3)],[(1,4),(2,4)],[(1,3),(2,4)],[(1,4),(2,3)]]
functions :: [a] -> [b] -> [[(a,b)]]
functions xs ys = [zip xs ys' | ys' <- replicateM (length xs) ys]
