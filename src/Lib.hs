module Lib
    where

import Math.Algebra.Group.PermutationGroup as P
import Math.Combinat.Tuples as T
import Data.Set as S
import Data.List as L
import Data.Map as M
import Control.Arrow ((>>>))

(.>) = flip (.)


swaps :: Int -> [[Int]]
swaps = (+ (-1)) .> T.tuples 2 .> fmap nub .> nub .> L.filter (\x -> length x == 2)
    where
    nub :: Ord a => [a] -> [a]
    nub = S.fromList .> S.elems
