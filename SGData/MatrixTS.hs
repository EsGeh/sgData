-- | This module exports a matrix type as well as some functions to work with it
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable, MultiParamTypeClasses, TypeSynonymInstances, FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts, ScopedTypeVariables #-}
module SGData.MatrixTS(
	-- * Types
	Matrix(),
	-- ** type aliases for indexes and width or height
	TupleIndex,IndexRow,IndexCol,MatrSize,Width,Height,
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
	{-LogOrigin,
	Origin(..),
	WithLog,Log(..),
	LogVal(..)-}
	) where
--import Card as Unary
--import Util.Vector2D
--import Text

--import SGData.Point

import SGCard
--import SGCard.Unary
--import SGData.Vector2D

import Data.Foldable(Foldable) -- hiding(concat,toList)
import qualified Data.Foldable as Fold hiding(sum,concat,toList)
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


type VecCol i dim a = Matrix i dim N1 a
type VecRow i dim a = Matrix i N1 dim a

-- type Vec a = VecRow N2 a
newtype Point i dim a = Point { fromPoint :: VecRow i dim a }
	deriving( Eq )
instance (Num a, Card dim) => MatrAdd (Point TupleIndex dim a) where
	a |+| b = Point $ fromPoint a |+| fromPoint b
instance (Eq a, Ord a, Ix i) => Ord (Point i dim a) where
	compare l r = compare (mGetRow 0 $ fromPoint l) (mGetRow 0 $ fromPoint r)
	{-
instance (Show a) => Show (Point dim a) where
	show p = show $ fromPoint p
	-}

instance (Eq a, Ix a, Ix i) => Ix (Point i N0 a) where
	range (l,r) = []
	index (l,r) p = error "index of point with zero-th dimension not defined!"
	inRange (l,r) p = True
{-
instance (Eq a, Ix a) => Ix (Point N1 a) where
	range (l,r) = map point1D $ range (pointX l, pointX r)
	index (l,r) p = index (pointX l, pointX r) $ pointX p
	inRange (l,r) p = inRange (pointX l, pointX r) $ pointX p --p `elem` mGetRow 0 $ fromPoint p
-}

allCombs :: [[a]] -> [[a]]
allCombs [] = []
allCombs [one] = do
	f <- one
	return $ [f]
allCombs list = do
	f <- head list
	rest <- (allCombs $ tail list)
	return $ [f] ++ rest

-- general
instance (Eq a, Ord a, Ix a, Ix (Point TupleIndex dimPred a), Card dimPred, MatrValidSize N1 (Succ dimPred)) => Ix (Point TupleIndex (Succ dimPred) a) where
	-- :: (Point dim a, Point dim a) -> [ Point dim a ]
	range (l,r) =  let (listL, listR) = (pointGetAll l, pointGetAll r) in
		map (fromJust . point (undefined :: Succ dimPred)) $ allCombs $ map range $ zip listL listR
		--firstDim
		where
			{-
			firstDim = range (firstDim' l, firstDim' r)
				where
					firstDim' :: Point (Succ dimPred) a -> Point N1 a
					firstDim' p = point1D $ head $ pointGetAll p
					--firstDim' p = point (undefined :: N1) $ [ head $ pointGetAll p ]
			-}
			{-otherDims = map fromPoint $ range (delOneDim l, delOneDim r)
				where
					--delOneDim :: Point (Succ dimPred) a -> Point dimPred a
					delOneDim p = (point (undefined :: dimPred) $ drop 1 $ pointGetAll p) :: Point dimPred a
					{-
					delOneDim p = withCard (pointGetDim p -1) $ \newDim ->
						 point newDim $ take (pointGetDim p -1) $ pointGetAll p
					-}
			-}
	index (l,r) p = index (pointX l, pointX r) $ pointX p
	inRange (l,r) p = inRange (pointX l, pointX r) $ pointX p --p `elem` mGetRow 0 $ fromPoint p
type Point0D a = Point N0 a
type Point1D a = Point N1 a
type Point2D a = Point N2 a
type Point3D a = Point N3 a

--pointX :: Point dim a -> a
pointX = mGet (point2D (0,0)) . fromPoint 
pointY = mGet (point2D (0,1)) . fromPoint
pointZ = mGet (point2D (0,2)) . fromPoint

pointGetDim p = pointY $ mGetSize $ fromPoint p
pointGetDimTS p = rowLength
	where (_,rowLength) = mGetSizeTS $ fromPoint p
pointGetAll p = mGetRow 0 $ fromPoint p
pointGet index = mGet (point2D (0,toInt index)) . fromPoint

point :: (Monad m, MatrValidSize N1 dim) => dim -> [a] -> m (Point i dim a)
point dim list = mFromListRow (n1,dim) [list] >>= return . Point

--point0D :: Point0D a
point0D = fromJust $ mFromListRow (n0,n0) []
point1D :: (a) -> Point1D i a
point1D (x) = fromJust $ point n1 [x]
point2D :: (a,a) -> Point2D i a
point2D (x,y) = fromJust $ point n2 [x,y]
point3D :: (a,a,a) -> Point3D i a
point3D (x,y,z) = fromJust $ point n3 [x,y,z]


-- |a matrix. The constructor is hidden, so it cannot be used directly - use 'm' or 'mUnsafe' instead
data Matrix i countRow countCol t = M (Array i t)
	deriving(Traversable, Eq) -- don't know how to implement that, so I am using the "deriving" clause, ...
-- |index to access elements in a matrix
--type MatrIndex = (IndexRow, IndexCol)
type TupleIndex = (IndexRow, IndexCol)
type IndexRow = Int
type IndexCol = Int

--type MatrSize = Point2D Int
type Width = Int
type Height = Int

fromM (M array) = array
--type Size t = Vec t

---------------------------------------------------------------------------------------
-- instance declarations: -------------------------------------------------------------
---------------------------------------------------------------------------------------
class MatrMult a b c | a b -> c where
	(|*|) :: a -> b -> c

class MatrAdd a where
	(|+|) :: a -> a -> a

infixl 8 |-| 
(|-|) :: (Num a, Card countRow, Card countCol)=> Matrix i countRow countCol a -> Matrix i countRow countCol a -> Matrix i countRow countCol a
l |-| r = l |+| ((-1) *| r)

infixl 9 *| -- scalar mult
(*|) :: (Num a, Card countRow, Card countCol) => a -> Matrix i countRow countCol a -> Matrix i countRow countCol a
s *| matr = m (mGetSizeTS matr) $ \pos -> s * mGet pos matr

instance (Card x, Card y, Card z, Num a) => MatrMult (Matrix TupleIndex x y a) (Matrix TupleIndex y z a) (Matrix TupleIndex x z a) where
	l |*| r = m (countRow,countCol) $ valFromIndex
		where
			--valFromIndex :: MatrIndex -> c
			valFromIndex pos = sum $ zipWith (*) (mGetRow (pointX pos) l) (mGetCol (pointY pos) r)
			countRow = undefined :: x
			countCol = undefined :: z 
instance (Card countRow, Card countCol, Num a) => MatrAdd (Matrix TupleIndex countRow countCol a) where
	l |+| r = m (countRow,countCol) $ valFromIndex
		where
			valFromIndex pos = mGet pos l + mGet pos r
			countRow = undefined :: countRow
			countCol = undefined :: countCol
--instance (Eq a) => Eq (Matrix i countRow countCol a) where
	
instance (Card dim, Num a) => Num (Matrix TupleIndex dim dim a) where
	(+) = (|+|)
	(*) = (|*|)
	abs matr = fmap abs matr
	signum matr = error "undefined"
	fromInteger val = m dim (fromIntegral . const val)
		where
			dim = (undefined :: dim, undefined :: dim)

{-
det matr = case mGetSize matr of
	(0,0) -> mGet (0,0) matr
-}

-- | show the matrix in a simple way
instance (Show t) => Show (Matrix TupleIndex countRow countCol t) where
	show = unlines . map show . mGetAllRows -- mGetAllIndexRow
-- |enables mapping a function over every element in the matrix
instance (Ix i) => Functor (Matrix i countRow countCol) where
	fmap f (M array) = M $ (fmap f) array
-- |enables to fold a matrix into a single value
instance (Ix i) => Foldable (Matrix i countRow countCol) where
	foldMap toMonoid (M array) = Fold.foldMap toMonoid array
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
	(TupleIndex -> a -> b) -> Matrix TupleIndex countRow countCol a -> Matrix TupleIndex countRow countCol b
mapWithIndex f mat = m (mGetSizeTS mat) (\index -> f index (mGet index mat))

m :: (Card countRow, Card countCol) => (countRow,countCol) -> (TupleIndex -> t) -> Matrix TupleIndex countRow countCol t
m size' f = M $ array ((0,0),size |-| (1,1)) [ (ind, f ind) | ind <- allIndices ]
	where
		allIndices = [ point2D (row,col) | row <-[0..(sizeX -1)], col <- [0..(sizeY -1)] ]
		(sizeX, sizeY) = (toInt sizeX', toInt sizeY')
		size = (point2D (toInt sizeX', toInt sizeY')) :: TupleIndex
		(sizeX', sizeY') = size'
		--size = point2D $ size' -- (toInt $ pointX size', toInt $ pointY size')

-- |creates a matrix from a list of lines. The result is packed into Maybe, because the input might be invalid
mFromListRow :: (Monad m, MatrValidSize countRow countCol) => (countRow,countCol) -> [[t]] -> m (Matrix TupleIndex countRow countCol t)
mFromListRow (height,width) listLines = if not (isValid listLines)
	then fail "wrong input format!"
	else (return $ m (height,width) (\index -> (listLines !! pointX index ) !! pointX index))
	where
		isValid listLines = Fold.foldl (\x y -> x && (length y==widthRT)) True listLines
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
mGetSizeTS :: (Ix i) => Matrix i countRow countCol t -> (countRow,countCol)
mGetSizeTS m = undefined

-- |retrieve the height of a matrix, that is the number of lines it consists of
--mGetHeight :: Matrix i countRow countCol t -> Height
mGetHeight = tupleX . mGetSize
-- |retrieve the width of a matrix, that is the number of columns it consists of
--mGetWidth :: Matrix i countRow countCol t -> Width
mGetWidth = tupleY . mGetSize


-- |retrieve the element by its index from the matrix
--mGet :: i -> Matrix i countRow countCol t -> t
mGet index (M array) = array ! index

-- |returns a submatrix
mGetSub (pos,size) matr = m size (\index -> mGet (index |+| pos) matr)
-- |returns a row of the matrix as a list
mGetRow indexRow matr = do
	indexCol <- mGetAllIndexCol matr
	let index = point2D (indexRow,indexCol)
	return $ mGet index matr
-- |returns a column of the matrix as a list
mGetCol indexCol matr = do
	indexRow <- mGetAllIndexRow matr
	let index = point2D (indexRow,indexCol)
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
--mGetWithOrigin :: i -> Matrix i countRow countCol t -> (t,i)
mGetWithOrigin index matr = (mGet index matr, index)

-- |returns an element, packed in the 'WithOriginMatr' Monad
{-
mGetWithLog :: i -> Matrix i countRow countCol t -> LogOrigin t
mGetWithLog index matr = do
	tell $ Log [(ValO index)]
	return $ val
		where val = mGet index matr
-}
mGetAllIndexRow matr = [0..(mGetHeight matr -1)]
mGetAllIndexCol matr = [0..(mGetWidth matr -1)]
mGetAllIndex matr = [(row,col) | row <- mGetAllIndexRow matr, col <- mGetAllIndexCol matr ]


-- |set the element at a specific index inside a matrix
--mSet :: (Card countRow,Card countCol) => i -> t -> Matrix i countRow countCol t -> Matrix i countRow countCol t
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

{-
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
	show (Log list) = Fold.foldl conc "" $ map show list
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
-}


---------------------------------------------------------------------------------------
-- internal functions -----------------------------------------------------------------
---------------------------------------------------------------------------------------
type Length = Int
{-arrayFromList :: Length -> [a] -> Array Length a
arrayFromList length list = listArray (0,(length-1)) list-}

tupleX = fst
tupleY = snd
