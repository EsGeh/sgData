{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
module SGData.Indexable.Classes(
	Listable,
	ListableCT,
	ListFL,
	fromList
) where

import SGCard
import Data.Array


class LE l r
instance LE Zero Zero
instance (LE l r) => LE (Succ l) (Succ r)
instance (LE l r) => LE l (Succ r)

class LT l r 
instance LT Zero N1
instance (LT l r) => LT (Succ l) (Succ r)
instance (LT l r) => LT l (Succ r)

class (Card size) => CardList size l where
	getCard :: (LT index size, Card n) => index -> l -> n

class (Ix i) => Indexable l i a | l -> i, l -> a where
	get :: i -> l -> a

{-
class (Indexable l i a, Ix i) => FixedSizeIndexable l i a | l -> i, l -> a where
	bounds :: l -> (i,i)
-}

class UpBounded l i | l -> i where
	upperBound :: l -> i

class (Card size) => UpBoundedCT l size i | l -> size, l -> i where

instance (Card size, UpBoundedCT l size Int) => UpBounded l Int where
	upperBound _ = toInt (undefined :: size)

class DownBounded l i | l -> i where
	lowerBound :: l -> i

class (Card size) => DownBoundedCT l size i | l -> size, l -> i  where

instance (Card size, DownBoundedCT l size Int) => DownBounded l Int where
	lowerBound _ = toInt (undefined :: size)

class BoundedCont l i | l -> i where
	bounds :: l -> (i,i)

class (Card min, Card max) => BoundedCT l min max i | l -> min, l -> max, l -> i

instance (Card min, Card max, BoundedCT l min max Int) => BoundedCont l Int where
	bounds _ = (lower, upper)
		where
			lower = toInt (undefined :: min)
			upper = toInt (undefined :: max)
{-
class (CardList N2 bounds) => BoundedCT l bounds i | l -> bounds, l -> i

instance (CardList N2 bounds, BoundedCT l bounds Int) => BoundedCont l Int where
	bounds _ = (lower, undefined)
		where
			lower = toInt $ getCard n0 (undefined :: bounds)
-}

{-
class (Listable l a) => Bounded l a | l -> a where
	bounds :: l -> (Int,Int)

class (Ix i, IFromCard i) => BoundedCT l size i a | l -> size, l -> i, l -> a where

class IFromCard i where
	fromCard :: (Card n) => n -> i
-}

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

instance ListableCT N2 (a,a) a where
instance ListableCT N3 (a,a,a) a 
instance ListableCT N4 (a,a,a,a) a 
instance ListableCT N5 (a,a,a,a,a) a 
instance ListableCT N6 (a,a,a,a,a, a) a 

newtype ListFL dim a = ListFL { flToList :: [a] }
	deriving( Show )
instance (Card size) => Listable (ListFL size a) a where
	toList = flToList
instance (Card size) => ListableCT size (ListFL size a) a

listFLFromList :: Card size => size -> [a] -> Maybe (ListFL size a)
listFLFromList size list = takeSafe (toInt size) list >>= return . ListFL

takeSafe n list = let l = take n list in
	if length l == n
		then Just l
		else Nothing where
	toList = flToList

{-
infixl 8 |+| -- :: a -> a -> a
x |+| y = 
-}

--class CompoundIndex dim ii i == ListableCT dim ii i
