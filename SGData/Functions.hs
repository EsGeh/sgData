{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, Rank2Types #-}
module SGData.Functions where

import SGData.Classes

import SGCard
import Data.Array
import SGData.ListStatic
--import Data.Foldable


zipWithCT :: (FromListCT lr c size, ToListCT l a size,ToListCT r b size) =>(a -> b -> c) -> l -> r -> lr 
zipWithCT f l r = fromListCT $ zipWith f (toListCT l) (toListCT r)

--zipCT :: (FromListCT lr (a, b) size, ToListCT l a size, ToListCT r b size) => l -> r -> lr 
zipCT :: (FromListCT lr (a,b) size, ToListCT l a size,ToListCT r b size) => l -> r -> lr 
zipCT = zipWithCT (,) --fromListCT $ (toListCT l) `zip` (toListCT r)

foldlCT ::  ToListCT l b size => (a -> b -> a) -> a -> l -> a
foldlCT f z l = foldl f z (toListCT l)

{-
instance (ListCT l a size) => Foldable l  where
	foldMap f l = undefined
-}

class (Container i a, Container i b, Container i c, Container i d)=> MatrBoundsContainer i a b c d
instance  (Container i a, Container i b, Container i c, Container i d)=> MatrBoundsContainer i a b c d

reifyMatrBounds :: Int -> Int -> Int -> Int -> (forall a b c d . (Container Int a, Container Int b, Container Int c, Container Int d) => (a,b) -> (c,d) -> res) -> res
reifyMatrBounds a b c d f = reify4 a b c d (\a b c d -> f (a,b) (c,d))

m :: (Ix i, Container i a, Container i b, Container i c, Container i d, MatrixClass m (i,i) i x ((a,b),(c,d))) => (a, b) -> (c, d) -> ((i,i) -> x) -> m
m (a, b) (c, d) f = fromFunction f


redDim :: (
	LessThan n (Succ dim),
	MultiIndex (Succ dim) ii i, --FromListCT ii i (Succ dim),
	MultiIndex dim ii' i, --ToListCT ii' i dim,
	ToFunction f ii y,
	FromFunction f' ii' y)
	=>
	n -> i -> f -> f'
--most general type:
--redDim :: (LessThan n (Succ dim), FromListCT ii i (Succ dim),ToListCT ii' i dim, ToFunction f ii y, FromFunction f' ii' y) =>n -> i -> f -> f'
redDim n i m = fromFunction func'
	where
		func' ii =  (toFunction m) (insertCT n i ii)


infixl 8 |+|
infixl 8 |-|
infixl 8 |*|
infixl 7 |/|
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

row :: (
	MultiIndex N2 ii i, --FromListCT ii i (Succ dim),
	MultiIndex N1 ii' i, --ToListCT ii' i dim,
	ToFunction f ii y,
	FromFunction f' ii' y)
	=>
	i -> f -> f'
row = redDim n0

col :: (
	MultiIndex N2 ii i, --FromListCT ii i (Succ dim),
	MultiIndex N1 ii' i, --ToListCT ii' i dim,
	ToFunction f ii y,
	FromFunction f' ii' y)
	=>
	i -> f -> f'
col = redDim n1


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

