{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module SGData.Indexable.Classes(
	Listable,
	ListableCT,
	ListFL,
	fromList
) where

--import SGCard
import Card2
--import SGData.Indexable.Card2
import Data.Array
import Data.Reflection
import Data.Proxy
import Data.Maybe(fromJust)


{-
class (Ix i) => Indexable l i a | l -> i, l -> a where
	get :: i -> l -> a
-}

-- up bounded: 
class UpBounded l i | l -> i where
	upperBound :: l -> i

class (Card size) => UpBoundedCT l size i | l -> size, l -> i where

-- down bounded: 
class DownBounded l i | l -> i where
	lowerBound :: l -> i

class (Card size) => DownBoundedCT l size i | l -> size, l -> i where
--class (Reifies size Int) => UpBoundedCT l size i | l -> size, l -> i where

-- has upper and lower boundaries:
class BoundedCont l i | l -> i where
	bounds :: l -> (i,i)

class (Card min, Card max) => BoundedCT l min max i | l -> min, l -> max, l -> i

instance (Card size, UpBoundedCT l size Int) => UpBounded l Int where
	upperBound x = fromCard (undefined :: size)
instance (Card size, DownBoundedCT l size Int) => DownBounded l Int where
	lowerBound x = fromCard (undefined :: size)
instance (Card min, Card max, BoundedCT l min max Int) => BoundedCont l Int where
	bounds _ = (lower, upper)
		where
			lower = fromCard (undefined :: min)
			upper = fromCard (undefined :: max)

class FromFunction f x y | f -> x, f -> y where
	fromFunction :: (x -> y) -> f

class ToFunction f x y | f -> x, f -> y where
	toFunction :: f -> x -> y

-- infixl 8 |+|
--a -> a -> a
--class MatrAdd a where
-- |+| :: a -> a -> a

mAdd :: forall f i a . (Num a, Ix i, ToFunction f i a, FromFunction f i a) => f -> f -> f
mAdd l r = fromFunction f
	where
		f :: i -> a 
		f i = (toFunction l) i + (toFunction r) i
{-
mAdd :: forall f a . (Num a, ToFunction f Int a, FromFunction f Int a) => f -> f -> f
mAdd l r = fromFunction f
	where
		f :: Int -> a
		f i = (toFunction l) i + (toFunction r) i
-}

data ListCTSize a size = ListCTSize { fromTestList :: [a] }
	deriving( Show )

createTestList :: forall a size . (Card size) => [a] -> Maybe (ListCTSize a size)
createTestList list = takeSafe (fromCard (undefined :: size)) list >>= return . ListCTSize

{-
test' i = withInt i func

func :: forall n . (Card n) => n -> Maybe String
func n = do 
	list <- (createTestList [1..] :: Maybe (ListCTSize Int n))
	return $ show $ (list `mAdd` list)
-}

instance (Card size) => UpBoundedCT (ListCTSize a size) size Int
instance (Card size) => ToFunction (ListCTSize a size) Int a where
	toFunction l i = case i < fromCard (undefined :: size) of
		False -> error "index error"
		True -> fromTestList l !! i
instance (Card size) => FromFunction (ListCTSize a size) Int a where
	fromFunction f = fromJust $ createTestList [ f i | i <- [0..(fromCard (undefined :: size)-1)] ]
{-
instance FromFunction (ListCTSize Int N3) Int Int where
	fromFunction f = fromJust $ createTestList [ f i | i <- [0..(fromCard (undefined :: N3)-1)] ]
-}

test' :: Maybe (ListCTSize Int N3)
test' = do
	l <- (createTestList [1..] :: Maybe (ListCTSize Int N3))
	return $ mAdd l l

{-
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
-}
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

newtype ListFL dim a = ListFL { flToList :: [a] }
	deriving( Show )
instance (Card size) => Listable (ListFL size a) a where
	toList = flToList
instance (Card size) => ListableCT size (ListFL size a) a

listFLFromList :: Card size => size -> [a] -> Maybe (ListFL size a)
listFLFromList size list = takeSafe (fromCard size) list >>= return . ListFL

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
