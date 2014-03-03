{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, Rank2Types #-}

import SGCard
import SGData.Classes
import SGData.Tensor
import SGData.ListStatic

import Data.Array

import Data.Reflection
import Data.Proxy

import Data.Maybe(fromJust)


test = reify (10) (show . f)
	where
		f :: forall n . (Container Int n) => n -> ListStatic Int (Succ n)
		f n = consCT 3 (fromListCT [1..] :: ListStatic Int n)

instance MultiIndex N1 (ListStatic Int N1) Int
instance MultiIndex N2 (ListStatic Int N2) Int

instance (MultiIndex dim ii i) => IndexChangeDim dim dim ii ii i
instance IndexChangeDim N2 N1 (ListStatic Int N2) (ListStatic Int N1) Int

class (Container (ii,ii) bounds, Container (ii',ii') bounds') => ChangeBounds ii ii' bounds bounds' | ii ii' bounds -> bounds'

--instance (Container (ListStatic Int N2) bounds, Container (ListStatic Int N1) bounds') => ChangeBounds (ListStatic Int N2) (ListStatic Int N1) bounds bounds'

{-
redDim :: forall n dim m m' ii ii' i a bounds bounds'.
	(Container ((Int,Int),(Int,Int)) bounds, LessThan n N2, IndexChangeDim N2 N1 (TensorClass N2 m ii i a bounds)
	=>
	n -> i -> m -> Tensor i a bounds'
--reduceDim :: forall n dim m m' ii ii' i a . (LessThan n N2, FromFunction m' ii' a, ToFunction m ii a) => n -> i -> m -> m'
-}

{-instance (Container (t,t) (l,r)) => Container t l where
	fromContainer x = fromContainer (undefined :: l)
instance (Container (t,t) (l,r)) => Container t r where
	fromContainer x = fromContainer (undefined :: r)-}

instance ToListCT Int Int N1 where
	toListCT i = [i]


--test2' = (reify 2 $ \n1 n2 -> show n1 n2)

{-
class Container2 t1 t2 c | c -> t1, c -> t2 where
	fromCont2_1 :: c -> t1
	fromCont2_2 :: c -> t2
-}

--instance (Reifies config (t1,t2)) => Container2 t1 t2 (Proxy config)

reify1 x = reify x

reify2 :: a -> b -> (forall l r . (Container a l, Container b r) => l -> r -> res) -> res
reify2 a b f = reify a (\n1 -> reify b (f n1))

reify3 :: a -> b -> c -> (forall t1 t2 t3. (Container a t1, Container b t2, Container c t3) => t1 -> t2 -> t3 -> res) -> res
reify3 a b c f = reify a (\n1 -> reify b (\n2 -> reify c (f n1 n2)))

reify4 :: a -> b -> c -> d -> (forall t1 t2 t3 t4 . (Container a t1, Container b t2, Container c t3, Container d t4) => t1 -> t2 -> t3 -> t4 -> res) -> res
reify4 a b c d f = reify a (\n1 -> reify b (\n2 -> reify c (\n3 -> reify d (f n1 n2 n3))))

{-
reifyMatrBounds :: MatrBounds (Int,Int) (Int,Int) -> (forall t1 t2 t3 t4 . (Container Int t1, Container Int t2, Container Int t3, Container Int t4) => MatrBounds (t1,t2) (t3,t4) -> res) -> res
reifyMatrBounds ((a,b),(c,d)) f = reify4 a b c d f'
	where
		f' a b c d = f $ matrBounds (a,b) (c,d)

--matrBounds a b c d = ((a,b),(c,d))
data MatrBounds iiMin iiMax = MatrBounds { boundsMin :: iiMin, boundsMax :: iiMax }
matrBounds = MatrBounds

class (Container Int rowS, Container Int colS, Container Int rowMax, Container Int colMax) => MatrBoundsClass rowS colS rowMax colMax
instance (Container Int rowS, Container Int colS, Container Int rowMax, Container Int colMax) => MatrBoundsClass rowS colS rowMax colMax 


test2 = reify4 0 0 2 2 (\sr sc maxRow maxCol -> show $ f sr sc maxRow maxCol)
	where
		f bounds = matr -- selRow 2 matr bounds
			where matr = t bounds

selRow :: (MatrBounds (rowS,colS) (rowMax,colMax))
	=> Int -> Tensor (Int,Int) a ((rowS,colS),(rowMax,colMax)) -> ((rowS,colS),(rowMax,colMax))-> Tensor Int a (colS,colMax) 
selRow i m bounds = fromFunction func'
	where
		func' ii =  (toFunction m) (insertCT n0 i ii)
{-
selRow :: (MatrBounds rowS colS rowMax colMax) 
	=> Int -> Tensor (Int,Int) a ((rowS,colS),(rowMax,colMax)) -> ((rowS,colS),(rowMax,colMax))-> Tensor Int a (colS,colMax) 
selRow i m bounds = fromFunction func'
	where
		func' ii =  (toFunction m) (insertCT n0 i ii)
-}

--t :: (Container Int dim, MultiIndex dim ii i, Container (ii,ii) bounds) => bounds -> Tensor ii Int bounds
t :: (Ix i, Container i a, Container i b, Container i c, Container i d) => MatrBounds (a,b) (c,d) -> Tensor (Int,Int) Int bounds
t bounds = tensor' bounds f
	where
		f ii = (get n0 ii) + (get n1 ii)

tensor' :: (Ix i, Container i a, Container i b, Container i c, Container i d) => bounds -> (i -> a) -> Tensor i a bounds
tensor' bounds f = fromFunction f
-}

--redTensor :: (LessThan n N2)=> n -> Int -> Tensor (Index N2) Int (Index2 N2) -> Tensor (Index N1) Int (Index2 N1)
redTensor n i t = fromFunction $ reduce n i $ toFunction t
	where
		--reduce :: n -> i -> (ii -> Int) -> (ii' -> Int)
		reduce = undefined


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
