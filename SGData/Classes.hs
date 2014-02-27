{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module SGData.Classes where

import SGCard

import Data.Array
import Data.Reflection
--import Data.Proxy
import Data.Maybe(fromJust)


-- up bounded: 
class UpBounded l i | l -> i where
	upperBound :: l -> i
 -- ct
class (Ix i, Container i size) => UpBoundedCT l size i | l -> size, l -> i where
--class (Card size) => UpBoundedCT l size i | l -> size, l -> i where

-- down bounded: 
class DownBounded l i | l -> i where
	lowerBound :: l -> i
 -- ct
class (Ix i, Container i size) => DownBoundedCT l size i | l -> size, l -> i where
--class (Reifies size Int) => UpBoundedCT l size i | l -> size, l -> i where

-- has upper and lower boundaries:
class BoundedCont l i | l -> i where
	bounds :: l -> (i,i)

class (Ix i, Container (i,i) bounds ) => BoundedCT l bounds i | l -> bounds, l -> i

-- homomorphism: (x -> y) |-->  f
class FromFunction f x y | f -> x, f -> y where
	fromFunction :: (x -> y) -> f

-- homomorphism:  f |--> (x -> y)
class ToFunction f x y | f -> x, f -> y where
	toFunction :: f -> x -> y

class (Container Int size) => ToListCT l a size | l -> a, l -> size where
	toListCT :: l -> [a]

class (Container Int size) => FromListCT l a size | l -> a, l -> size where
	fromListCT :: [a] -> l

class ListCT l a size | l -> a, l -> size where	
	get :: (LessThan n size) => n -> l -> a

--instance ToListCT a a N1 where toListCT = id
instance ToListCT (a,a) a N2 where toListCT (x,y) = [x,y]
instance ToListCT (a,a,a) a N3 where toListCT (x,y,z) = [x,y,z]
instance FromListCT (a,a) a N2 where fromListCT [x,y] = (x,y)
instance FromListCT (a,a,a) a N3 where fromListCT [x,y,z] = (x,y,z)

{-
instance (Ix a) => MultiIndex N2 (a,a) a
instance (Ix a) => MultiIndex N1 a a
-}

instance (ToListCT l a size) => ListCT l a size where
	get n l = toListCT l !! fromCard n

-- these are just shortcuts:
class (Ix ii, Ix i, FromListCT ii i dim, ToListCT ii i dim, ListCT ii i dim) =>
	MultiIndex dim ii i | ii -> dim, ii -> i

class (BoundedCT m bounds ii, MultiIndex N2 ii i, ToFunction m ii a, FromFunction m ii a) =>
	Matrix m ii i a bounds | m -> ii, m -> a, m -> bounds

-- to be moved to SGCard:
class (Container Int n, Container Int m) => LessThan n m 
instance (Container Int n) => LessThan Zero (Succ n)
instance (LessThan n m) => LessThan (Succ n) (Succ m)


--instance BoundedCT c (min,max) i => (DownBoundedCT c min i) where
--instance (DownBoundedCT c min i, UpBoundedCT c max i) => BoundedCT c (min,max) i where

 -- if ct bounded => also bounded
 -- this would be nice, but needs compiler flag "-UndecidableInstances", which is considered dangerous...
{-
instance (UpBoundedCT l size Int) => UpBounded l Int where
	upperBound x = fromCard (undefined :: size)
instance (DownBoundedCT l size Int) => DownBounded l Int where
	lowerBound x = fromCard (undefined :: size)
instance (BoundedCT l min max Int) => BoundedCont l Int where
	bounds _ = (lower, upper)
		where
			lower = fromCard (undefined :: min)
			upper = fromCard (undefined :: max)
-}

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
consCT :: (FromListCT l a size, ToListCT l1 a (Succ size)) => a -> l1 -> l
consCT x list = fromListCT $ x:(toListCT list)


insertCT :: (LessThan n (Succ size), FromListCT l' a (Succ size), ToListCT l a size) => n -> a -> l -> l'
insertCT n x = fromListCT . insert x . toListCT
	where
		insert :: a -> [a] -> [a]
		insert x l = take i l ++ [x] ++ drop i l
		i = fromContainer n



{-
class Listable l a | l -> a where
	toList :: l -> [a]

class FromListable l a | l -> a where
	fromList :: [a] -> l

class (Card size, Listable l a) => ListableCT size l a | l size -> a where

--instance Listable (a) a where toList (a1) = [a1]
instance Listable (a,a) a where toList (a1,a2) = [a1, a2]
instance Listable (a,a,a) a where toList (a1,a2,a3) = [a1, a2, a3]
instance Listable (a,a,a,a) a where toList (a1,a2,a3,a4) = [a1, a2, a3, a4]
instance Listable (a,a,a,a,a) a where toList (a1,a2,a3,a4,a5) = [a1, a2, a3, a4, a5]
instance Listable (a,a,a,a,a, a) a where toList (a1,a2,a3,a4,a5,a6) = [a1, a2, a3, a4, a5, a6]

instance Listable [a] a where
	toList l = l

{-
instance ListableCT N2 (a,a) a where
instance ListableCT N3 (a,a,a) a 
instance ListableCT N4 (a,a,a,a) a 
instance ListableCT N5 (a,a,a,a,a) a 
instance ListableCT N6 (a,a,a,a,a, a) a 
-}

{-
instance ListableCT N2 (a,a) a where
instance ListableCT N3 (a,a,a) a 
instance ListableCT N4 (a,a,a,a) a 
instance ListableCT N5 (a,a,a,a,a) a 
instance ListableCT N6 (a,a,a,a,a, a) a 
-}
-}

takeSafe n list = let l = take n list in
	if length l == n
		then Just l
		else Nothing 
