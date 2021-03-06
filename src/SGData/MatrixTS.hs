-- | This module exports a matrix type as well as some functions to work with it
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable, MultiParamTypeClasses, TypeSynonymInstances #-}
module SGData.MatrixTS(
	-- * Types
	Matrix(),
	-- ** type aliases for indexes and width or height
	MatrIndex,IndexRow,IndexCol,MatrSize,Width,Height,
	-- * Matrix Pseudo Constructors
	m, mFromListRow,
	-- * Getter
	mGet,
	mGetSize, mGetHeight,mGetWidth,
	-- ** Lists of Indices
	mGetAllIndexRow,mGetAllIndexCol,mGetAllIndex,
	-- ** extract specific parts from the matrix
	mGetSub,mGetRow,mGetCol,
	mGetAllRows,mGetAllCols,
	-- ** Monadic Getters
	mGetWithOrigin,
	-- * Setter
	mSet,
	-- * enhanced mapping
	mapWithIndex,
	-- * Special Monads (experimental)
	LogOrigin,
	Origin(..),
	WithLog,Log(..),
	LogVal(..)
	) where
--import Card as Unary
--import Util.Vector2D
--import Text

import SGCard.Card
import SGCard.Unary
import SGData.Vector2D

import Data.Foldable hiding(concat,toList)
import Data.Foldable as Fold hiding(concat,toList)
import Control.Applicative
import Data.Traversable
import Data.List hiding(foldl,foldr)
--import qualified Data.List as List
import Prelude hiding(foldl,foldr,Left,Right)
import Data.Monoid 
import Data.Maybe
import Control.Monad.Writer
import Data.Ratio
import Data.Array

import Debug.Trace

-- |a matrix. The constructor is hidden, so it cannot be used directly - use 'm' or 'mUnsafe' instead
data Matrix countRow countCol t = M (Array MatrIndex t)
	deriving(Traversable) -- don't know how to implement that, so I am using the "deriving" clause, ...
-- |index to access elements in a matrix
type MatrIndex = (IndexRow, IndexCol)
type IndexRow = Int
type IndexCol = Int

type MatrSize = Vec Int
type Width = Int
type Height = Int


--type Size t = Vec t

---------------------------------------------------------------------------------------
-- instance declarations: -------------------------------------------------------------
---------------------------------------------------------------------------------------
-- | show the matrix in a nice table like representation
{-instance (Show t) => Show (Matrix countRow countCol t) where
	show m@(M array) = show $ (runRenderMeth $ renderMeth) (0,0) listCol
		where
			renderMeth :: (Show t) => RenderMethod [[t]] TextBlock
			renderMeth = horizontalWith (filledBlock "|") combPStd (repeat (vertical combPStd (repeat justBlock)))
			listCol = [ mGetCol indexCol m | indexCol <- mGetAllIndexCol m ]
	{-show m@(M array) = show $ (runRenderMeth $ renderMeth) (0,0) listCol
		where
			renderMeth :: (Show t) => RenderMethod [[t]] TextBlock
			renderMeth = horizontalWith (filledBlock "|") (repeat (vertical (repeat justBlock)))
			listCol = [ mGetCol indexCol m | indexCol <- mGetAllIndexCol m ]-}
			
		{-concat $ intersperse "\n" $ elems $ fmap (prettyShow " | " ((fromIntegral maxLength)%1) 0 ) $ listLines
			where
				maxLength = Fold.maximum $ fmap (length . show) m
				prettyShow = Pretty.showContainer "" "" " " " " Pretty.LeftJust
				-}
-}
-- |enables mapping a function over every element in the matrix
instance Functor (Matrix countRow countCol) where
	fmap f (M array) = M $ (fmap f) array
-- |enables to fold a matrix into a single value
instance Foldable (Matrix countRow countCol) where
	foldMap toMonoid (M array) = foldMap toMonoid array
{-instance Traversable Matrix where
	--traverse :: Applicative f => (a -> f b) -> Matrix a -> f (Matrix b)
	traverse f (M listLines) = traverse (mUnsafe . traverse f) listLines
	-}
-- |a matix of functions can be applied on a matrix of values
{-
instance Applicative (Matrix countRow countCol) where
	--pure :: a -> Matrix countRow countCol a
	pure val = M $ array (\_ -> val)
	--(<*>) :: Matrix (a->b) -> Matrix a -> Matrix b
	matrF <*> matrVal = mapWithIndex f matrVal
		where
			f ind x = (mGet ind matrF) x
		--[ [ (mGet (indexRow,indexCol) matrF $ (mGet (indexRow,indexCol) matrVal)) | indexCol <- (mGetAllIndexCol matrVal) ] | indexRow <- (mGetAllIndexRow matrVal)]
		-}

-- |fmap allows to map a function over a 'Foldable'
-- but the function does not know the position of the element it is applied on.
-- 'mapWithIndex' solves this problem
mapWithIndex :: (Card countRow,Card countCol) =>
	(MatrIndex -> a -> b) -> Matrix countRow countCol a -> Matrix countRow countCol b
mapWithIndex f mat = m (mGetSizeTS mat) (\index -> f index (mGet index mat))

m :: (Card countRow, Card countCol) => (countRow,countCol) -> (MatrIndex -> t) -> Matrix countRow countCol t
m size' f = M $ array ((0,0),size |-| (1,1)) [ (ind, f ind) | ind <- allIndices ]
	where
		allIndices = [ (row,col) | row <-[0..(vecX size -1)], col <- [0..(vecY size -1)] ]
		size = (toInt $ vecX size', toInt $ vecY size')

-- |creates a matrix from a list of lines. The result is packed into Maybe, because the input might be invalid
mFromListRow :: (Monad m, MatrValidSize countRow countCol) => (countRow,countCol) -> [[t]] -> m (Matrix countRow countCol t)
mFromListRow (height,width) listLines = if not (isValid listLines)
	then fail "wrong input format!"
	else (return $ m (height,width) (\(row,col) -> (listLines !! row) !! col))
	where
		isValid listLines = foldl (\x y -> x && (length y==widthRT)) True listLines
		(heightRT,widthRT) = (toInt height, toInt width)

class (Card w, Card h) => MatrValidSize w h
instance MatrValidSize N0 N0	
instance (Card w, Card h) => MatrValidSize (Succ w) (Succ h)
{-
m :: [[t]] -> Maybe (Matrix t)
m listLines = if (isValid listLines)
	then Just $ M $ arrayFromList height $ map (arrayFromList width) listLines
	else Nothing
	where
		isValid listLines = foldl (\x y -> x && (length y==width)) True listLines
		height = length listLines
		width = length $ listLines !! 0
-}

-- |creates a matrix from a list of lists. If the input is invalid, 'error' is called
{-
mUnsafe :: [[t]] -> Matrix t
mUnsafe listLines = fromMaybe (error "failed to create Matrix from list") $ m listLines
-}

-- |create a square matrix
{-mSqr :: [[t]] -> Maybe (Matrix t)
mSqr = m-}
	-- to do: check if input makes up a valid square matrix
mGetSize (M array) = (snd $ bounds array) |+| (1,1)
mGetSizeTS :: Matrix countRow countCol t -> (countRow,countCol)
mGetSizeTS m = undefined

-- |retrieve the height of a matrix, that is the number of lines it consists of
mGetHeight :: Matrix countRow countCol t -> Height
mGetHeight = vecX . mGetSize
-- |retrieve the width of a matrix, that is the number of columns it consists of
mGetWidth :: Matrix countRow countCol t -> Width
mGetWidth = vecY . mGetSize


-- |retrieve the element by its index from the matrix
mGet :: MatrIndex -> Matrix countRow countCol t -> t
mGet index (M array) = array ! index

-- |returns a submatrix
mGetSub (pos,size) matr = m size (\index -> mGet (index |+| pos) matr)
-- |returns a row of the matrix as a list
mGetRow indexRow matr = do
	indexCol <- mGetAllIndexCol matr
	let index = (indexRow,indexCol)
	return $ mGet index matr
-- |returns a column of the matrix as a list
mGetCol indexCol matr = do
	indexRow <- mGetAllIndexRow matr
	let index = (indexRow,indexCol)
	return $ mGet index matr

-- |returns a list of all rows in then matrix
mGetAllRows matr = do
	indexRow <- mGetAllIndexRow matr
	return $ mGetRow indexRow matr
-- |returns a list of all columns in then matrix
mGetAllCols matr = do
	indexCol <- mGetAllIndexCol matr
	return $ mGetCol indexCol matr

--mGetLines (M lines) = lines

-- |returns an element, packed in the 'WithOriginMatr' Monad
mGetWithOrigin :: MatrIndex -> Matrix countRow countCol t -> (t,MatrIndex)
mGetWithOrigin index matr = (mGet index matr, index)

-- |returns an element, packed in the 'WithOriginMatr' Monad
mGetWithLog :: MatrIndex -> Matrix countRow countCol t -> LogOrigin t
mGetWithLog index matr = do
	tell $ Log [(ValO index)]
	return $ val
		where val = mGet index matr
mGetAllIndexRow matr = [0..(mGetHeight matr -1)]
mGetAllIndexCol matr = [0..(mGetWidth matr -1)]
mGetAllIndex matr = [(row,col) | row <- mGetAllIndexRow matr, col <- mGetAllIndexCol matr ]


-- |set the element at a specific index inside a matrix
mSet :: (Card countRow,Card countCol) => MatrIndex -> t -> Matrix countRow countCol t -> Matrix countRow countCol t
mSet index val matr = mapWithIndex maybeSet matr
	where
		maybeSet index' val' = if index' == index then val else val'


---------------------------------------------------------------------------------------
-- Monads  ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- |This Monad enables you to make calculations based on values that possibly come from a matrix,
-- while logging where the values come from
-- Example:
--
-- >>> let test = mUnsafe [[1,2],[3,4]] -- creating a test matrix
--
-- >>> runWriter $ mGetWithOrigin (0,1) test
-- (2,(0,1))
--
-- first entry is the value, second is the log of this value (in fact packed into 'Origin'):
-- 
-- >>> :t runWriter $ mGetWithOrigin (0,1) ma
-- :: (Integer, Log (Origin MatrIndex))

type LogOrigin t = Writer (Log (Origin MatrIndex)) t
type WithLog t = Writer (Log (LogVal t)) t

-- |defines a log, which is considered a sequence of any type.
-- if printed it looks like this:
--
--	thing0 -> thing1 -> thing2 -> ...
{-type Log logType = [logType]
showLog :: (Show logType) => Log logType -> String
showLog listOfEntries = foldl conc "" $ map show listOfEntries 
	where
		conc "" y = y
		conc x y = x ++ " -> " ++ y-}
newtype Log logType = Log { getLog :: [logType] }
	deriving(Monoid)
instance (Show logType) => Show (Log logType) where
	show (Log list) = foldl conc "" $ map show list
		where
			conc "" y = y
			conc x y = x ++ " -> " ++ y

-- |The encodes that the origin might be given:
data Origin t =
	ValO t -- ^ origin
	| NilO -- ^ origin not known
	deriving(Show)
{-instance (Show t) => Show (Origin t) where
	show NilO = ""
	show (ValO val) = show val-}

data LogVal t = NilL | ValL t | Fun String
instance (Show t) => Show (LogVal t) where
	show NilL = ""
	show (ValL val) = show val
	show (Fun str) = str


---------------------------------------------------------------------------------------
-- internal functions -----------------------------------------------------------------
---------------------------------------------------------------------------------------
type Length = Int
{-arrayFromList :: Length -> [a] -> Array Length a
arrayFromList length list = listArray (0,(length-1)) list-}
