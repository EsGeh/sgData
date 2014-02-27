-- | This module exports a matrix type as well as some functions to work with it
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module SGData.Tensor where

--import SGData.Card2
import SGData.Classes

import SGCard

import Data.Array
import Data.Reflection
import Data.Proxy

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

instance (Ix i, Container (i,i) bounds) => BoundedCT (Tensor i a bounds) bounds i

tensor :: forall i a bounds . (Ix i, Container (i,i) bounds) => bounds -> (i -> a) -> Tensor i a bounds
tensor bounds f = fromFunction f


testTensor :: Tensor (Int,Int) Int ((N0,N0),(N2,N2))
testTensor = fromFunction (\(x,y) -> x+y) 

testOneDim f = reify (0,4) func -- this opens a context, in which bounds are statically fixed
	where
		func bounds = show $ 2 *| (tensor bounds f)

testTwoDim f = reify (((0,0),(4,4)) :: ((Int,Int),(Int,Int))) func -- this opens a context, in which bounds are statically fixed
	where
		func bounds = show $ tensor bounds f
