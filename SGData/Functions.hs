{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, Rank2Types #-}
module SGData.Functions where

import SGData.Classes

import SGCard
import Data.Array


class (Container i a, Container i b, Container i c, Container i d)=> MatrBoundsContainer i a b c d
instance  (Container i a, Container i b, Container i c, Container i d)=> MatrBoundsContainer i a b c d

reifyMatrBounds :: Int -> Int -> Int -> Int -> (forall a b c d . (Container Int a, Container Int b, Container Int c, Container Int d) => (a,b) -> (c,d) -> res) -> res
reifyMatrBounds a b c d f = reify4 a b c d (\a b c d -> f (a,b) (c,d))


infixl 8 |+|
infixl 8 |-|
infixl 8 |*|
infixl 8 |/|
infixl 8 *|
infixl 8 /|
infixl 8 |*
infixl 8 |/

l |+| r = mAdd l r
l |-| r = mSub l r
l |*| r = mMul l r
l |/| r = mDiv l r
s *| r = mScalarMult s r
s /| r = mScalarMult (1/s) r

-- flipped parameters:
(|*) :: forall f i a . (Ix i, Num a, ToFunction f i a, FromFunction f i a) => f -> a -> f
(|*) = flip (*|)
(|/) :: forall f i a . (Ix i, Fractional a, ToFunction f i a, FromFunction f i a) => f -> a -> f
(|/) = flip (/|)


mOp :: forall f i a . (Ix i, ToFunction f i a, FromFunction f i a) => (a -> a -> a) -> f -> f -> f
mOp f l r = fromFunction f'
	where
		f' :: i -> a 
		f' i = (toFunction l) i `f` (toFunction r) i

mAdd :: (Num a, Ix i, ToFunction f i a, FromFunction f i a) =>f -> f -> f
mAdd = mOp (+)
mSub :: (Num a, Ix i, ToFunction f i a, FromFunction f i a) =>f -> f -> f
mSub = mOp (-)
mMul :: (Num a, Ix i, ToFunction f i a, FromFunction f i a) =>f -> f -> f
mMul = mOp (*)
mDiv :: (Fractional a, Ix i, ToFunction f i a, FromFunction f i a) =>f -> f -> f
mDiv = mOp (/)


reduceDim :: forall n dim m m' ii ii' i a . (LessThan n (Succ dim), MultiIndex (Succ dim) ii i, MultiIndex dim ii' i, FromFunction m' ii' a, ToFunction m ii a) => n -> i -> m -> m'
--reduceDim :: (LessThan n N2, Matrix m ii i a (min,max), ListCT m' a ) => n -> i -> m -> m'
reduceDim n i f = fromFunction func'
	where
		func' :: ii' -> a
		func' ii' = func ii
			where
				ii = insertCT n i ii'
		func = (toFunction f)

--mMatrMul :: (Num y, ListCT x t N2, ToFunction f x y, ToFunction f1 x y,FromFunction f2 x y) =>f1 -> f -> f2
mMatrMul f g = fromFunction res
	where
		res ii = funcF ii + funcG ii --funcF (row,col) * funcG col (row,col)
			where
				(row, col) = (get n0 ii,  get n1 ii)
		funcF = (toFunction f)
		funcG = (toFunction g)

mScalarMult :: forall f i a . (Num a, Ix i, ToFunction f i a, FromFunction f i a) => a -> f -> f
mScalarMult s v = fromFunction f
	where
		f i = s * (toFunction v) i


--insertCT :: forall a b la lb size . (ToListCT la a size, FromListCT lb b (Succ size)) => ([a] -> [b]) -> la -> lb
--consCT :: (Container Int size, ToListCT la a size, FromListCT lb a (Succ size)) => a -> la -> lb
consCT :: (Container Int size, FromListCT l' a (Succ size), ToListCT l a size) => a -> l -> l'
consCT x list = fromListCT $ x:(toListCT list)


insertCT :: (LessThan n (Succ size), FromListCT l' a (Succ size), ToListCT l a size) => n -> a -> l -> l'
insertCT n x = fromListCT . insert x . toListCT
	where
		insert :: a -> [a] -> [a]
		insert x l = take i l ++ [x] ++ drop i l
		i = fromContainer n

