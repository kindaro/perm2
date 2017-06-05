module Lib
    where

import Math.Algebra.Group.PermutationGroup as P
import Math.Combinat.Tuples as T
import Data.Set as S
import Data.List as L
import Data.Map as M
import Control.Arrow ((>>>))

(.>) = flip (.)

type L2 a = [[a]]

nub2 :: Ord a => L2 a -> L2 a
nub2 =  fmap nub' .> nub'
    where
    nub' :: Ord a => [a] -> [a]
    nub' = S.fromList .> S.elems

lengthEq n xs = length xs == n
filterLength n = L.filter (lengthEq n)

swaps :: Int -> L2 Int
swaps = T.tuples1 2 .> nub2 .> filterLength 2

daisy2 = fmap (p . pure) dials
    where dials = [[1..4], [4..7]]

compositions xs n = fmap (xs!!) <$> c n (length xs)
    where
    c k n = concatMap (`T.tuples` (n - 1)) [1..k]

cy2 :: Int -> [L2 Integer]
cy2 = nub2 . fmap toCycles . fmap product . compositions daisy2
