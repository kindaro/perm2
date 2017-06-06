{-# Language
    OverlappingInstances
  , FlexibleInstances
  , TemplateHaskell
  #-}

module Lib
    where

import Math.Algebra.Group.PermutationGroup as P
import Math.Combinat.Tuples as T
import Data.Set as S
import Data.List as L
import Data.Map as M
import Control.Arrow ((>>>))
import Control.Comonad
import Data.Function.Memoize

(.>) = flip (.)

type L2 a = [[a]]

nub' :: Ord a => [a] -> [a]
nub' = S.fromList .> S.elems

nub2 :: Ord a => L2 a -> L2 a
nub2 =  fmap nub' .> nub'

lengthEq n xs = length xs == n
filterLength n = L.filter (lengthEq n)

swaps :: Int -> L2 Int
swaps = T.tuples1 2 .> nub2 .> filterLength 2

daisy2 = fmap (p . pure) dials
    where dials = [[1..4], [4..7]]

compositions xs n = fmap (xs!!) <$> c n (length xs)
    where
    c k n = concatMap (`T.tuples` (n - 1)) [1..k]

newtype Indexed k v = Indexed (k, v)

instance Eq v => Eq (Indexed k v) where
    Indexed (_, v1) == Indexed (_, v2) = v1 == v2

instance Ord v => Ord (Indexed k v) where
    Indexed (_, v1) `compare` Indexed (_, v2) = v1 `compare` v2

instance Functor (Indexed k) where
    fmap f (Indexed (k, v)) = Indexed (k, f v)

instance Monoid k => Applicative (Indexed k) where
    pure x = Indexed (mempty, x)
    Indexed (_, f) <*> Indexed (k, v) = Indexed (k, f v)

instance (Monoid k) => Comonad (Indexed k) where
    extract (Indexed (_, v)) = v
    extend f = pure . f

indexate f = fmap (\x -> Indexed (x, f x))

instance Ord a => Monoid (Permutation a) where
    mempty = 1
    mappend = (*)

cy2 :: Int -> [Indexed [Permutation Integer] [[Integer]]]
cy2 = L.filter criterion . indexate (product .> toCycles) . compositions daisy2
    where
    criterion = extract . fmap innerCriterion
    innerCriterion cycles = lengthEq 1 cycles && (and . fmap (lengthEq 2)) cycles

cy2memoized :: Int -> [Indexed [Permutation Integer] [[Integer]]]
cy2memoized = L.filter criterion . indexate (product .> toCycles) . compositions daisy2
    where
    criterion :: Memoizable a => Indexed [Permutation Integer] [[a]] -> Bool
    criterion = extract . fmap (memoize innerCriterion)
    innerCriterion :: [[a]] -> Bool
    innerCriterion cycles = lengthEq 1 cycles && (and . fmap (lengthEq 2)) cycles

instance (Show k, Show v) => Show (Indexed k v) where
    show (Indexed (k,v)) = show k ++ " @ " ++ show v

instance (Show k, Show v) => Show [Indexed k v] where
    show = intercalate "\n" . fmap show

swapsFromDaisy2 = nub' $ cy2memoized 14
