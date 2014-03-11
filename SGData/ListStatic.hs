{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}
module SGData.ListStatic where

--import SGData
import SGData.Classes
import SGCard
import Data.Maybe(fromJust)
import Data.Reflection
import Data.Proxy

import Data.Array

data ListStatic a size = ListStatic { fromListStatic :: [a] }
	deriving( Eq, Ord )

listStatic :: forall a size . (Container Int size) => [a] -> Maybe (ListStatic a size)
listStatic list = takeSafe (fromContainer (undefined :: size)) list >>= return . ListStatic

{-
data ReifyStaticList1 x1 = ReifyStaticList1 { sl1_x1 :: x1 }
data ReifyStaticList2 x1 x2 = ReifyStaticList2 { sl2_x1 :: x1, sl2_x2 :: x2 }

-- how else could I encode static lists?!
instance (Container t l, Container t r) => Container (ListStatic t N2) (ReifyStaticList2 l r) where
	fromContainer tuple = fromListCT [ fromContainer $ sl2_x1 tuple, fromContainer $ sl2_x2 tuple ]
-}

-- Static lists as index
instance MultiIndex N0 N0 (ListStatic Int N1) Int
instance MultiIndex N0 N1 (ListStatic Int N2) Int


instance (Container Int size, Show a) => Show (ListStatic a size) where
	show l = "N" ++ show (fromContainer (undefined :: size)) ++ show (fromListStatic l)

-- has static size:
--instance (Container Int size) => UpBoundedCT (ListStatic a size) Int size 
instance (Container Int max) => BoundedCT (ListStatic a (Succ max)) Int N0 max

-- isomorphic to a list:
instance (Container Int max) => ToListCT (ListStatic a (Succ max)) a N0 max where
	toListCT = fromListStatic
instance (Container Int max) => FromListCT (ListStatic a (Succ max)) a N0 max where
	fromListCT = fromJust . listStatic
	--fromListCT = ListStatic
instance (Container Int max) => ListCT (ListStatic a (Succ max)) a N0 max where
	get n l = toListCT l !! fromContainer n

-- isomorphic to a function:
instance (Container Int size) => ToFunction (ListStatic a size) Int a where
	toFunction l i = case i < fromContainer (undefined :: size) of
		False -> error "index error"
		True -> fromListStatic l !! i
instance (Container Int size) => FromFunction (ListStatic a size) Int a where
	fromFunction f = fromJust $ listStatic [ f i | i <- [0..(fromContainer (undefined :: size)-1)] ]

-- can be used as an index:
instance Ix (ListStatic Int N2) where
	range (l,r) = map (fromListCT . listFromTuple) $ range (l', r')
		where
			(l',r') = ((get n0 l, get n1 l), (get n0 r, get n1 r))
			listFromTuple (x,y) = [x,y]
	index (l,r) i = index (l',r') i'
		where
			(l',r') = ((get n0 l, get n1 l), (get n0 r, get n1 r))
			i' = (get n0 i, get n1 i)
	inRange (l,r) i = inRange (l',r') i'
		where
			(l',r') = ((get n0 l, get n1 l), (get n0 r, get n1 r))
			i' = (get n0 i, get n1 i)

instance Ix (ListStatic Int N1) where
	range (l,r) = map (fromListCT . listFromTuple) $ range (l', r')
		where
			(l',r') = ((get n0 l), (get n0 r))
			listFromTuple x = [x]
	index (l,r) i = index (l',r') i'
		where
			(l',r') = ((get n0 l), (get n0 r))
			i' = get n0 i
	inRange (l,r) i = inRange (l',r') i'
		where
			(l',r') = ((get n0 l), (get n0 r))
			i' = get n0 i


takeSafe n list = let l = take n list in
	if length l == n
		then Just l
		else Nothing 
