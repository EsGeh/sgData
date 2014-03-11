{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

import SGCard
import SGData

import Data.Array

import Data.Reflection
import Data.Proxy

import Data.Maybe(fromJust)


testMatrMul :: Matr Int Int N0 (N0,N0) (N1,N1)
testMatrMul = mMatrMul a b 
	where
		a = mFromList n0 (n0,n0) (n1,n2) [[3,2,1],[1,0,2]] :: Matr Int Int N0 (N0,N0) (N1,N2)
		b = mFromList n0 (n0,n0) (n2,n1) [[1,2],[0,1],[4,0]] :: Matr Int Int N0 (N0,N0) (N2,N1)


test3 = reifyMatrBounds 0 0 2 2 (\l r-> show $ f l r)
	where
		f :: forall a b c d . MatrBoundsContainer Int a b c d => (a, b) -> (c, d) -> Vector Int Int N0 a c
		f (a, b) (c, d) =
			row 2 $
			matr $
			(\(x,y) -> x*y) 
			where 
				matr = mTensorInt (a, b) (c, d)


test2' = reify4 0 0 2 2 (\a b c d -> show $ f (a, b) (c, d))
	where
		f :: forall a b c d . (Container Int a, Container Int b, Container Int c, Container Int d) => (a, b) -> (c, d) -> Tensor Int Int N0 N0 a c -- Tensor (Int,Int) Int ((a,b),(c,d))
		f (a, b) (c, d) = row 2 matr 
			where matr = m n0 (a, b) (c, d) (\(x,y) -> x*y) :: Tensor (Int,Int) Int N0 N1 (a,b) (c,d)


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
