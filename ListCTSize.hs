{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

import SGCard
import SGData.Classes
import SGData.Tensor
import SGData.ListStatic

import Data.Reflection

import Data.Maybe(fromJust)


{-
type Index n = ListStatic Int n
type Index2 n = (Index n, Index n)
type Index3 n = (Index n, Index n, Index n)

instance MultiIndex N2 (ListStatic Int N2) Int
instance MultiIndex N1 (ListStatic Int N1) Int
-}

reduceDim :: forall n dim m m' ii ii' i a . (LessThan n N2, Matrix m ii a, ListCT m' ii' ) => n -> i -> m -> m'
--reduceDim :: forall n dim m m' ii ii' i a . (LessThan n N2, FromFunction m' ii' a, ToFunction m ii a) => n -> i -> m -> m'
redDim n i m = fromFunction func'
	where
		func' :: ii' -> a
		func' ii' = func ii
			where
				ii = insertCT n i ii'
		func = (toFunction m)

--t :: (Container Int dim, MultiIndex dim ii i, Container (ii,ii) bounds) => bounds -> Tensor ii Int bounds
t :: Container ((Int,Int),(Int,Int)) bounds =>bounds -> Tensor (Int,Int) Int bounds
t bounds = tensor bounds f 
	where
		f ii = (get n0 ii) + (get n1 ii)
		--range = undefined --(fromListCT [n0,n0], fromListCT [n2,n2]) -- :: (ListStatic Int N2, ListStatic Int N2)

--redTensor :: (LessThan n N2)=> n -> Int -> Tensor (Index N2) Int (Index2 N2) -> Tensor (Index N1) Int (Index2 N1)
redTensor n i t = fromFunction $ reduce n i $ toFunction t
	where
		--reduce :: n -> i -> (ii -> Int) -> (ii' -> Int)
		reduce = undefined

{-
testX = reify (((create2 0 0), (create2 2 2)) :: Index2 N2) (show . f) 
	where
		f :: (Reifies bounds (Index2 N2) ) => Proxy bounds -> Tensor (Index N1) Int bounds' -- (Index N0, Index N2)
		f bounds = reduceDim n0 0 $ undefined -- t bounds
		temp :: (Container (Index2 N2) bounds) => bounds -> Tensor (Index N2) Int bounds
		temp bounds = t bounds {-:: Tensor (Index2 N2) Int bounds-}
-}

--instance Container (Proxy (Index2 N2)) bounds

--usesRedDim :: (Container (Index2 N1) bounds) => Tensor (Index N1) Int bounds
--usesRedDim :: (Reifies bounds (Index2 N2) ) => bounds -> Tensor (Index N1) Int (Index2 N1)
{-usesRedDim :: (Container (Index2 N1) bounds) => bounds -> Tensor (Index N1) Int bounds
usesRedDim bounds = redTensor n0 0 $ tensor (fromContainer bounds) (\i -> (get n0 i) + (get n1 i))-- (undefined :: Tensor (Index N2) Int (N0,N2))-}


create1 :: a -> ListStatic a N1
create1 x = fromJust $ listStatic [x]

create2 :: a -> a -> ListStatic a N2
create2 x y = fromJust $ listStatic [x,y]

test' :: forall n . Container Int n => n -> ListStatic Int n
test' _ = fromJust $ do
	l <- (listStatic [1..] :: Maybe (ListStatic Int n))
	r <- (listStatic [1..] :: Maybe (ListStatic Int n))
	return $ l |+| r

test1 = test' n1
test6 = test' n6

testNicer = reify 10 (show . test')

testNicer' = reify (10,20) (show . fromContainer)
testNicer'' = reify (create2 1 2) (show . fromContainer)
