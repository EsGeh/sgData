{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
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
class (Ix i, Container i size) => UpBoundedCT l i size | l -> size, l -> i where
--class (Card size) => UpBoundedCT l size i | l -> size, l -> i where

-- down bounded: 
class DownBounded l i | l -> i where
	lowerBound :: l -> i
 -- ct
class (Ix i, Container i size) => DownBoundedCT l i size | l -> size, l -> i where
--class (Reifies size Int) => UpBoundedCT l size i | l -> size, l -> i where

-- has upper and lower boundaries:
class BoundedCont l i | l -> i where
	bounds :: l -> (i,i)

class (Ix i, Container (i,i) bounds ) => BoundedCT l i bounds | l -> bounds, l -> i

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


-- Make ListCTs instances of Ix

-- this would be nice, but needs compiler flag "-UndecidableInstances", which is considered dangerous...
{-
instance (ToListCT l a size) => ListCT l a size where
	get n l = toListCT l !! fromCard n
-}


-- these are just shortcuts:
class (Ix ii, Ix i, FromListCT ii i dim, ToListCT ii i dim, ListCT ii i dim) =>
	MultiIndex dim ii i | ii -> dim, ii -> i

class (BoundedCT t ii bounds, MultiIndex dim ii i, ToFunction t ii a, FromFunction t ii a) =>
	TensorClass dim t ii i a bounds | t -> ii, t -> a, t -> bounds

class (TensorClass N2 m ii i a bounds) =>
	MatrixClass m ii i a bounds

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

