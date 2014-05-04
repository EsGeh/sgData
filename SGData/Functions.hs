{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, Rank2Types #-}
module SGData.Functions where

import SGData.Classes

import SGCard
import Data.Array
import SGData.ListStatic
import SGData.Tuple
--import Data.Foldable


-- | work with fixed size lists:

consCT :: (FromListCT l' a min (Succ max), ToListCT l a min max) => a -> l -> l'
consCT x list = fromListCT $ x:(toListCT list)


insertCT :: (InBounds min (Succ max) n, FromListCT l' a min (Succ max), ToListCT l a min max) => n -> a -> l -> l'
insertCT n x = fromListCT . insert x . toListCT
	where
		insert :: a -> [a] -> [a]
		insert x l = take i l ++ [x] ++ drop i l
		i = fromContainer n

zipWithCT :: (FromListCT lr c min max, ToListCT l a min max, ToListCT r b min max) =>(a -> b -> c) -> l -> r -> lr 
zipWithCT f l r = fromListCT $ zipWith f (toListCT l) (toListCT r)

zipCT :: (FromListCT lr (a,b) min max, ToListCT l a min max, ToListCT r b min max) => l -> r -> lr 
zipCT = zipWithCT (,) --fromListCT $ (toListCT l) `zip` (toListCT r)

foldlCT ::  ToListCT l b min max => (a -> b -> a) -> a -> l -> a
foldlCT f z l = foldl f z (toListCT l)

{-
instance (ListCT l a size) => Foldable l  where
	foldMap f l = undefined
-}

-- work with functions: 

class (Container i a, Container i b, Container i c, Container i d)=> MatrBoundsContainer i a b c d
instance  (Container i a, Container i b, Container i c, Container i d)=> MatrBoundsContainer i a b c d

reifyMatrBounds :: Int -> Int -> Int -> Int -> (forall a b c d . (Container Int a, Container Int b, Container Int c, Container Int d) => (a,b) -> (c,d) -> res) -> res
reifyMatrBounds a b c d f = withCard4 a b c d (\a b c d -> f (a,b) (c,d))

-- |generic matrix constructor
m :: (Ix i, MatrixClass m minDim (i,i) i x (a,b) (c,d)) => minDim -> (a, b) -> (c, d) -> ((i,i) -> x) -> m
m minDim (a, b) (c, d) f = fromFunction f

-- |
-- TODO: Type-Check
mFromList :: (
		MatrBoundsContainer Int a b c d,
		MatrixClass m minDim (Int, Int) Int y (a,b) (c,d)
	)
	=>
	minDim -> (a,b) -> (c,d) -> [[y]] -> m
mFromList minDim l r list = m minDim l r (f list)
	where
		f list (row,col) = list !! row !! col

-- matrix:
mMatrMul :: forall y l r res u v w minDim . (
	Num y, -- LessThan N0 dim, LessThan N1 dim,
	Container Int u, Container Int v, Container Int w,
	MatrixClass l minDim (Int,Int) Int y (N0,N0) (u,v), 
	MatrixClass r minDim (Int,Int) Int y (N0,N0) (v,w), 
	MatrixClass res minDim (Int,Int) Int y (N0,N0) (u,w))
	=>
	l -> r -> res 
mMatrMul f g = fromFunction $ res
	where
		res (irow,icol) = foldlCT (+) 0 $ --temp f g (iRow,iCol)
			(zipWithCT (*)
				(row irow f :: ListStatic y (Succ v))
				(col icol g :: ListStatic y (Succ v)) :: ListStatic y (Succ v))


-- work with functions (not necessarily bounded!) :
--
--
--
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


mScalarMult :: forall f i a . (Num a, Ix i, ToFunction f i a, FromFunction f i a) => a -> f -> f
mScalarMult s v = fromFunction f
	where
		f i = s * (toFunction v) i

{-
mRow :: (
	MatrBoundsContainer (i,i) a b c d,
	MatrixClass m (i,i) i y ((a,b),(c,d))
	FromListCT i i (a,c)
	)
	=> i -> m -> l
mRow = undefined
-}

{-
mRow :: (
	MatrixClass m minDim ii i y (minRow,minCol) (maxRow,maxCol),
	FromListCT f' y min max)
	=>
	i -> f -> f'
mRow = redDim n0
-}

-- extract rows/columns from a two dimensional indexable structure:
row :: (
	MultiIndex N0 N1 ii i, --FromListCT ii i (Succ dim),
	MultiIndex N0 N0 ii' i, --ToListCT ii' i dim,
	ToFunction f ii y,
	FromFunction f' ii' y)
	=>
	i -> f -> f'
row = redDim n0

col :: (
	MultiIndex N0 N1 ii i, --FromListCT ii i (Succ dim),
	MultiIndex N0 N0 ii' i, --ToListCT ii' i dim,
	ToFunction f ii y,
	FromFunction f' ii' y)
	=>
	i -> f -> f'
col = redDim n1

-- extract orthonormal planes from a multi dimensionally indexable structure
redDim :: (
	InBounds minDim (Succ maxDim) n,
	MultiIndex minDim (Succ maxDim) ii i, --FromListCT ii i (Succ dim),
	MultiIndex minDim maxDim ii' i, --ToListCT ii' i dim,
	ToFunction f ii y,
	FromFunction f' ii' y)
	=>
	n -> i -> f -> f'
--most general type would be:
--redDim :: (LessThan n (Succ dim), FromListCT ii i (Succ dim),ToListCT ii' i dim, ToFunction f ii y, FromFunction f' ii' y) =>n -> i -> f -> f'
redDim n i m = fromFunction func'
	where
		func' ii =  (toFunction m) (insertCT n i ii)
