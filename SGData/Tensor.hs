-- | This module exports a matrix type as well as some functions to work with it
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
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


newtype Tensor i a minDim maxDim min max = T { fromTensor :: Array i a }
	deriving( Show )

instance (Ix i, Container Int minDim, Container Int maxDim, Container i min, Container i max) => FromFunction (Tensor i a minDim maxDim min max) i a where
	fromFunction f = T $ array bounds list --array bounds list
		where
			bounds = fromContainer (undefined :: (min,max))
			list = [ (i, f i) | i <- ((range (minBound, (maxBound)))) ]
			minBound = fst bounds
			maxBound = snd bounds

instance (Ix i, Container Int minDim, Container Int maxDim, Container i min, Container i max) => ToFunction (Tensor i a minDim maxDim min max) i a where
	toFunction (T a) i = a ! i

instance (Ix i, Container Int minDim, Container Int maxDim, Container i min, Container i max) => BoundedCT (Tensor i a minDim maxDim min max) i min max

-- just to make shure:
instance (
	Container Int minDim, Container Int maxDim,
	MultiIndex minDim maxDim ii i, 
	Container ii min,
	Container ii max)
	=> 
	TensorClass minDim maxDim (Tensor ii a minDim maxDim min max) ii i a min max

instance (MultiIndex minDim (Succ minDim) ii i, Container ii min, Container ii max) => MatrixClass (Tensor ii a minDim (Succ minDim) min max) minDim ii i a min max

mTensorInt :: (
	MatrBoundsContainer Int rowMin colMin rowMax colMax
	)
	=>
	(rowMin,colMin) -> (rowMax, colMax) -> ((Int,Int) -> a) -> Matr Int a N0 (rowMin,colMin) (rowMax, colMax)
mTensorInt lower upper f = m n0 lower upper f 
{-
mTensorInt :: (
	MatrBoundsContainer Int rowMin colMin rowMax colMax
	)
	=>
	minDim -> (rowMin,colMin) -> (rowMax, colMax) -> ((Int,Int) -> a) -> Matr Int a minDim (rowMin,colMin) (rowMax, colMax)
mTensorInt minDim lower upper f = m minDim lower upper f 
-}


type Matr i a minDim lower upper = Tensor (i,i) a minDim (Succ minDim) lower upper
type Vector i a minDim lower upper = Tensor i a minDim minDim lower upper
