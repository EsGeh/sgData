-- | This module exports a matrix type as well as some functions to work with it
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module SGData.Tensor where

--import SGData.Card2
import SGData.Classes
import SGData.Functions
import SGData.Tuple

import SGData.ListStatic

import SGCard

import Data.Array
import Data.Reflection
import Data.Proxy


-- matrix:

-- |
-- TODO: Type-Check
mFromList :: (
		MatrBoundsContainer Int a b c d,
		MatrixClass m (Int, Int) Int y ((a,b), (c,d))
	)
	=>
	(a,b) -> (c,d) -> [[y]] -> m
mFromList l r list = m l r (f list)
	where
		f list (row,col) = list !! row !! col


mMatrMul :: forall y l r res u v w . (
	Num y, -- LessThan N0 dim, LessThan N1 dim,
	Container Int u, Container Int v, Container Int w,
	MatrixClass l (Int,Int) Int y ((N0,N0),(u,v)), 
	MatrixClass r (Int,Int) Int y ((N0,N0),(v,w)), 
	MatrixClass res (Int,Int) Int y ((N0,N0),(u,w)))
	=>
	l -> r -> res 
mMatrMul f g = fromFunction $ res
	where
		res (irow,icol) = foldlCT (+) 0 $ --temp f g (iRow,iCol)
			(zipWithCT (*)
				(row irow f :: ListStatic y (Succ v))
				(col icol g :: ListStatic y (Succ v)) :: ListStatic y (Succ v))

newtype Tensor i a bounds = T { fromTensor :: Array i a }
	deriving( Show )

{-
instance Show (Tensor i a bounds) where
	show t = 
-}

instance (Ix i, Container (i,i) bounds) => FromFunction (Tensor i a bounds) i a where
	fromFunction f = T $ array bounds list --array bounds list
		where
			bounds = fromContainer (undefined :: bounds)
			list = [ (i, f i) | i <- ((range (minBound, (maxBound)))) ]
			minBound = fst bounds
			maxBound = snd bounds

instance (Ix i, Container (i,i) bounds) => ToFunction (Tensor i a bounds) i a where
	toFunction (T a) i = a ! i

instance (Ix i, Container (i,i) bounds) => BoundedCT (Tensor i a bounds) i bounds

-- just to make shure:
instance (Container Int dim, MultiIndex dim ii i, Container (ii,ii) bounds) => TensorClass dim (Tensor ii a bounds) ii i a bounds

instance (MultiIndex N2 ii i, Container (ii,ii) bounds) => MatrixClass (Tensor ii a bounds) ii i a bounds


mTensorInt :: (MatrBoundsContainer Int rowMin colMin rowMax colMax) => (rowMin,colMin) -> (rowMax, colMax) -> ((Int,Int) -> a) -> Matr Int a (rowMin,colMin) (rowMax, colMax)
mTensorInt lower upper f = m lower upper f 


type Matr i a lower upper = Tensor (i,i) a (lower,upper)
type Vector i a lower upper = Tensor i a (lower, upper)


{-
tensor :: forall i a bounds . (Ix i, Container (i,i) bounds) => bounds -> (i -> a) -> Tensor i a bounds
tensor bounds f = fromFunction f
-}


--instance (Reifies config i) => Container i (Proxy config)

{-
testTensor :: Tensor (Int,Int) Int ((N0,N0),(N2,N2))
testTensor = fromFunction (\(x,y) -> x+y) 

testOneDim f = reify ((0,4) :: (Int,Int)) func -- this opens a context, in which bounds are statically fixed
	where
		func bounds = show $ 2 *| (tensor bounds f)

testTwoDim f = reify (((0,0),(4,4)) :: ((Int,Int),(Int,Int))) func -- this opens a context, in which bounds are statically fixed
	where
		func bounds = show $ tensor bounds f
-}
