{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

import Temp

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

test2 = reify4 0 0 2 2 (\a b c d -> show $ f a b c d)
	where
		f :: (Container Int a, Container Int b, Container Int c, Container Int d) => a -> b -> c -> d -> Tensor (Int,Int) Int ((a,b),(c,d))
		f a b c d = matr  -- selRow 2 matr bounds
			where matr = m a b c d (\(x,y) -> x*y)

m :: (Ix i, Container i a, Container i b, Container i c, Container i d, MatrixClass m (i,i) i x ((a,b),(c,d))) => a -> b -> c -> d -> ((i,i) -> x) -> m
m a b c d f = fromFunction f


type Matr i lower upper = Tensor (i,i) i (lower,upper)
type Vector i lower upper = Tensor i i (lower, upper)

test3 = reifyMatrBounds 0 0 2 2 (\l r-> show $ f l r)
	where
		f :: forall a b c d . MatrBoundsContainer Int a b c d => (a, b) -> (c, d) -> Vector Int a c
		f (a, b) (c, d) = selRow (a, b) (c, d) 2 matr 
			where 
				matr = m' (a, b) (c, d) (\(x,y) -> x*y) :: Matr Int (a,b) (c,d)

test2' = reify4 0 0 2 2 (\a b c d -> show $ f (a, b) (c, d))
	where
		f :: forall a b c d . (Container Int a, Container Int b, Container Int c, Container Int d) => (a, b) -> (c, d) -> Tensor Int Int (a,c) -- Tensor (Int,Int) Int ((a,b),(c,d))
		f (a, b) (c, d) = selRow (a, b) (c, d) 2 matr 
			where matr = m' (a, b) (c, d) (\(x,y) -> x*y) :: Tensor (Int,Int) Int ((a,b),(c,d))

m' :: (Ix i, Container i a, Container i b, Container i c, Container i d, MatrixClass m (i,i) i x ((a,b),(c,d))) => (a, b) -> (c, d) -> ((i,i) -> x) -> m
m' (a, b) (c, d) f = fromFunction f

selRow ::(Ix i, Container i a, Container i b, Container i c, Container i d, MatrixClass m (i,i) i x ((a,b),(c,d)), TensorClass N1 m' i i x (a,c))
	=> (a,b) -> (c,d) -> i -> m -> m'
selRow (a,b) (c,d) i m = fromFunction func'
	where
		func' ii =  (toFunction m) (insertCT n0 i ii)


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
