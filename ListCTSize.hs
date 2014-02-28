{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

import SGData
import SGCard
import Data.Maybe(fromJust)
import Data.Reflection
import Data.Proxy

import Data.Array

data ListCTSize a size = ListCTSize { fromTestList :: [a] }
	deriving( Eq, Ord )

instance (Show size, Show a) => Show (ListCTSize a size) where
	show l = {-show (undefined :: size) ++ -} show (fromTestList l)

createTestList :: forall a size . (Container Int size) => [a] -> Maybe (ListCTSize a size)
createTestList list = takeSafe (fromContainer (undefined :: size)) list >>= return . ListCTSize

{-
test' i = withInt i func

func :: forall n . (Card n) => n -> Maybe String
func n = do 
	list <- (createTestList [1..] :: Maybe (ListCTSize Int n))
	return $ show $ (list `mAdd` list)
-}

instance (Container Int size) => UpBoundedCT (ListCTSize a size) Int size 
instance (Container Int size) => BoundedCT (ListCTSize a size) Int (N0,size)
instance (Container Int size) => ToListCT (ListCTSize a size) a size where
	toListCT = fromTestList
instance (Container Int size) => FromListCT (ListCTSize a size) a size where
	fromListCT = ListCTSize

instance (Container Int size) => ToFunction (ListCTSize a size) Int a where
	toFunction l i = case i < fromContainer (undefined :: size) of
		False -> error "index error"
		True -> fromTestList l !! i
instance (Container Int size) => FromFunction (ListCTSize a size) Int a where
	fromFunction f = fromJust $ createTestList [ f i | i <- [0..(fromContainer (undefined :: size)-1)] ]

--instance (Container (Int,Int) bounds) => Container (ListCTSize Int size, ListCTSize Int size) bounds

--data WrapRange = 


instance (Container a size) => ListCT (ListCTSize a size) a size where
	get n l = toListCT l !! fromContainer n

instance Ix (ListCTSize Int N2) where
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

instance Ix (ListCTSize Int N1) where
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

type Index n = ListCTSize Int n
type Index2 n = (Index n, Index n)
type Index3 n = (Index n, Index n, Index n)

instance MultiIndex N2 (ListCTSize Int N2) Int
instance MultiIndex N1 (ListCTSize Int N1) Int

t :: (Container (Index2 N2) bounds) => bounds -> Tensor (Index N2) Int bounds
t bounds = tensor range f 
	where
		f ii = (get n0 ii) + (get n1 ii)
		range = undefined --(fromListCT [n0,n0], fromListCT [n2,n2]) -- :: (ListCTSize Int N2, ListCTSize Int N2)

{-
testX = reify (((create2 0 0), (create2 2 2)) :: Index2 N2) (show . f) 
	where
		f :: (Reifies bounds (Index2 N2) ) => Proxy bounds -> Tensor (Index N1) Int bounds' -- (Index N0, Index N2)
		f bounds = reduceDim n0 0 $ undefined -- t bounds
		temp :: (Container (Index2 N2) bounds) => bounds -> Tensor (Index N2) Int bounds
		temp bounds = t bounds {-:: Tensor (Index2 N2) Int bounds-}
-}

instance Container (Proxy (Index2 N2)) bounds

usesRedDim :: (Reifies bounds (Index2 N2)) => Tensor (Index N2) Int bounds
usesRedDim = reduceDim n0 0 $ undefined


create1 :: a -> ListCTSize a N1
create1 x = fromJust $ createTestList [x]

create2 :: a -> a -> ListCTSize a N2
create2 x y = fromJust $ createTestList [x,y]

test' :: forall n . Container Int n => n -> ListCTSize Int n
test' _ = fromJust $ do
	l <- (createTestList [1..] :: Maybe (ListCTSize Int n))
	r <- (createTestList [1..] :: Maybe (ListCTSize Int n))
	return $ l |+| r

test1 = test' n1
test6 = test' n6

testNicer = reify 10 (show . test')
