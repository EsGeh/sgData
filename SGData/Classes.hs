{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module SGData.Classes where

import SGCard

import Data.Array
import Data.Reflection
--import Data.Proxy
import Data.Maybe(fromJust)


-- has upper and lower boundaries:
class BoundedCont l i | l -> i where
	bounds :: l -> (i,i)

class (Ix i, Container i min, Container i max) => BoundedCT l i min max | l -> min, l -> max, l -> i



-- homomorphism: (x -> y) |-->  f
class FromFunction f x y | f -> x, f -> y where
	fromFunction :: (x -> y) -> f

-- homomorphism:  f |--> (x -> y)
class ToFunction f x y | f -> x, f -> y where
	toFunction :: f -> x -> y



class (BoundedCT l Int min max) => ToListCT l a min max | l -> a, l -> min, l -> max where
	toListCT :: l -> [a]

class (BoundedCT l Int min max) => FromListCT l a min max | l -> a, l -> min, l -> max where
	fromListCT :: [a] -> l

class (BoundedCT l Int min max) => ListCT l a min max | l -> a, l -> min, l -> max where	
	get :: (InBounds min max n) => n -> l -> a


class (LessOrEqual min n, LessOrEqual n max, Container Int min, Container Int max, Container Int n) => InBounds min max n
instance (LessOrEqual min n, LessOrEqual n max) => InBounds min max n


-- Make ListCTs instances of Ix

-- this would be nice, but needs compiler flag "-UndecidableInstances", which is considered dangerous...
{-
instance (ToListCT l a size) => ListCT l a size where
	get n l = toListCT l !! fromCard n
-}


-- these are just shortcuts:
class (Ix ii, Ix i, FromListCT ii i min max, ToListCT ii i min max, ListCT ii i min max) =>
	MultiIndex min max ii i | ii -> min, ii -> max, ii -> i

class (BoundedCT t ii min max, MultiIndex minDim maxDim ii i, ToFunction t ii a, FromFunction t ii a) =>
	TensorClass minDim maxDim t ii i a min max | t -> ii, t -> a, t -> minDim, t -> maxDim, t -> min, t -> max

class (TensorClass minDim (Succ minDim) m ii i y min max) =>
	MatrixClass m minDim ii i y min max

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

