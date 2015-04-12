{-# LANGUAGE ScopedTypeVariables #-}
module SGData.TreeZipper(
	-- * zipper data type
	Zipper(..),
	-- ** apply functions to subelements
	changeFocus, changeContext,
	applyOnFocus, applyOnContext,
	-- *** monadic
	changeFocusM, changeContextM,
	applyOnFocusM, applyOnContextM,
	-- * context data type
	TreeContext(..),
	-- * construct a zipper
	--zipper,
	zipperTop,
	-- * getters
	depth, index,
	-- * move around in trees
	walkPath, moveZipper, moveIfPossible,
	pathFromZipper,
	top, leftBottom,
	up, down, left, right,
	allDown,
	-- * context dependent tree manipulations
	-- ** unfold
	unfoldSeesZipper,
	-- ** change trees
	updateNodesSeesZipper,
	{-
	applyZipperFOnFocus,
	applyZipperFOnAllChildren,
	applyZipperFOnRight,
	-}
	mapSeesZipper,
	-- * monadic versions of context dependent tree manipulations
	-- ** unfold
	unfoldSeesZipperM,
	-- ** change trees
	updateNodesSeesZipperM,
	{-
	applyZipperFOnFocusM,
	applyZipperFOnAllChildrenM,
	applyZipperFOnRightM,
	-}
	mapSeesZipperM,
	-- * small types
	Direction(..), Path,
) where

import SGData.Tree
import SGData.Util

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State


{-
newtype ZipperT m a = ZipperT {
	fromZipperT :: StateT (Maybe (TreeContext a)) m a
}
	--deriving (Monad, MonadTrans)
instance Monad m => Monad (ZipperT m) where
	return = ZipperT . return
	f >>= g = ZipperT $ fromZipperT f >>= (fromZipperT . g)
-}

-- | a zipper is a node plus some information about its context (its position in the tree)
data Zipper a = Zipper {
	focus :: Node a, -- ^ retrieve the node from a zipper
	context :: Maybe (TreeContext a) -- ^ retrieve the context from a zipper
}
	deriving(Show)

{-
mapOnFocus f z = z{ focus = f (focus z) }
mapOnContext f z = z{ context = f (context z) }
-}

changeFocus = nonMonadic changeFocusM
changeContext = nonMonadic changeContextM
applyOnFocus = nonMonadic applyOnFocusM
applyOnContext = nonMonadic applyOnContextM

changeFocusM f z = do
	newN <- f z
	return $ Zipper newN (context z)

changeContextM f z = do
	newCxt <- f z
	return $ Zipper (focus z) newCxt

applyOnFocusM f = changeFocusM (f . focus)
applyOnContextM f = changeContextM (f . context)

data TreeContext a = TreeContext {
	childIndex :: Int,
	parentCxt :: Maybe (TreeContext a),
	parentTree :: Tree a
}
	deriving(Show)

mapToChildIndex f context = context{ childIndex = f (childIndex context) }
mapToParentCxt f context = context{ parentCxt = f (parentCxt context) }
mapToParentTree f context = context{ parentTree = f (parentTree context) }

data Direction = Up | Down Int
type Path = [Direction]

depth :: Num n => Zipper a -> n
depth z = case (up z) of
	Nothing -> 0
	Just parent -> depth parent + 1
index z = case context z of
	Nothing -> 0
	Just ctxt -> childIndex ctxt

{-
mapZipperF :: (Tree a-> Tree a) -> Zipper a -> Maybe (Zipper a)
mapZipperF f zipper = (top zipper)
-}

walkPath :: Path -> Zipper a -> Maybe (Zipper a)
walkPath list zipper = case list of
	[] -> Just zipper
	(dir:restDirections) ->
		walkPath restDirections
		=<< moveZipper dir zipper

moveIfPossible dir zipper = maybe zipper id (moveZipper dir zipper)

moveZipper :: Direction -> Zipper a -> Maybe (Zipper a)
moveZipper dir zipper = case dir of
	Up -> up zipper
	Down index -> down index zipper

pathFromZipper :: Zipper a -> Path
pathFromZipper zipper = case context zipper of
	Nothing -> []
	Just ctxt ->
		--TCDown { childIndex = i } ->
		let i = childIndex ctxt
		in
			(pathFromZipper (fromJust $ up zipper)) ++ [Down i]

--zipper = Zipper 
zipperTop tree = Zipper tree Nothing

top :: Zipper a -> Zipper a
top zipper = case up zipper of
	Nothing -> zipper
	Just parent -> parent
leftBottom zipper = case down 0 zipper of
	Nothing -> zipper
	Just leftMostChild -> leftBottom leftMostChild

safeIndexing index list = if index < 0 then Nothing else
	safeIndexing' index list

safeIndexing' index list = case list of
	[] -> Nothing
	(x:xs) -> case index of
		0 -> Just x
		_ -> safeIndexing' (index-1) xs

down :: Int -> Zipper a -> Maybe (Zipper a)
down int z = do
	child <- safeIndexing int (children $ focus z)
	let parentTree = applyOnChildren (delFromIndex int) $ focus z
	return $ Zipper
		child
		(Just $ TreeContext {
			childIndex = int,
			parentCxt = context z,
			parentTree = parentTree })

down' :: Int -> Zipper a -> Maybe (Zipper a)
down' int zipper = case children (focus zipper) of
	[] -> Nothing
	children -> if int < length children && int >= 0
		then Just $ Zipper
			(children !! int)
			(Just $ TreeContext {
				childIndex = int,
				parentCxt = context zipper,
				parentTree = parentTree })
		else Nothing
	where
		parentTree = applyOnChildren (delFromIndex int) $ focus zipper

allDown :: Zipper a -> [Zipper a]
allDown zipper = allDown 0
	where
		allDown index = case down index zipper of
			Nothing -> []
			Just child -> child : allDown (index+1) 

up :: Zipper a -> Maybe (Zipper a)
up z = do
	cxt <- context z
	let
		upperNode =
			applyOnChildren (addFromIndex (childIndex cxt) (focus z)) (parentTree $ cxt)
	return $ Zipper
		upperNode
		(parentCxt cxt)

left z = do
	cxt <- context z
	up z >>= down (childIndex cxt - 1)
right z = do
	cxt <- context z
	up z >>= down (childIndex cxt + 1)

{- |> unfoldSeesZipper f startVal

creates a tree using f, whereas the result may depend on branches which have been calculated already.
Branches are created top down from left to right.
If they have not, the value of the zipper will be Nothing.
-}
unfoldSeesZipper :: forall param a . (param -> Zipper (Maybe a) -> (a,[param])) -> param -> Tree a
unfoldSeesZipper f params =
	runIdentity $ unfoldSeesZipperM (\p -> return . f p) params

-- | monadic version of 'unfoldSeesZipper'
unfoldSeesZipperM :: Monad m => (param -> Zipper (Maybe a) -> m (a,[param])) -> param -> m (Tree a)
unfoldSeesZipperM f params =
	return . fmap fromJust 
	=<<
		(unfoldSeesZipperM' f params $
		(zipperTop $ blankInfiniteTree))
	where
		blankInfiniteTree = unfoldTree (const (Nothing, repeat Nothing)) Nothing


unfoldSeesZipperM' :: forall m param a . Monad m => (param -> Zipper (Maybe a) -> m (a,[param])) -> param -> Zipper (Maybe a) -> m (Tree (Maybe a))
unfoldSeesZipperM' f params zipper = do
	(val, subParams) <- f params zipper :: m (a, [param])
	return . node (Just val) =<< calcSubTrees [] subParams
	where
		calcSubTrees :: [Tree (Maybe a)] -> [param] -> m [Tree (Maybe a)]
		calcSubTrees _ [] = return []
		calcSubTrees list (param:restParams) = do
			unfoldFirst <- unfoldSeesZipperM' f param subZipper
			rec <- calcSubTrees (list++[unfoldFirst]) restParams
			return $ unfoldFirst: rec
			where
				subZipper =
					applyOnContext (liftM calcCtxt) $
					fromJust $
					down currentIndex $
					zipper
				calcCtxt :: TreeContext (Maybe a) -> TreeContext (Maybe a)
				calcCtxt = mapToParentTree newParentTree
					where
						newParentTree :: Tree (Maybe a) -> Tree (Maybe a)
						newParentTree pT = flip applyOnChildren pT $ \children ->
							list ++ drop (length list) children :: [Node (Maybe a)]
				{-
				calcCtxt TCTop = TCTop
				calcCtxt ctxt@TCDown{ parentTree = pT } =
					ctxt{ parentTree = newParentTree }
					where
						newParentTree :: Tree (Maybe a)
						newParentTree = flip applyOnChildren pT $ \children ->
							list ++ drop (length list) children :: [Node (Maybe a)]
				-}
				currentIndex = length list

{- |> updateNodesSeesZipper f zipper

recursively applies f to the whole subtree of the node the zipper points to
-}
updateNodesSeesZipper :: (Zipper a -> Node a) -> Zipper a -> Zipper a
updateNodesSeesZipper = nonMonadic updateNodesSeesZipperM

-- |monadic version of 'updateNodesSeesZipper'
updateNodesSeesZipperM :: Monad m => (Zipper a -> m (Node a)) -> Zipper a -> m (Zipper a)
updateNodesSeesZipperM f zipper' =
	applyZipperFOnAllChildrenM (return . focus <=< updateNodesSeesZipperM f)
	=<< changeFocusM f zipper'

{- |> applyZipperFOnFocus f zipper

returns a new Zipper where f has been applied to all children
-}
applyZipperFOnAllChildren :: (Zipper a -> Node a) -> Zipper a -> Zipper a
applyZipperFOnAllChildren =
	nonMonadic applyZipperFOnAllChildrenM

-- |
applyZipperFOnRight :: (Zipper a -> Node a) -> Zipper a -> Zipper a
applyZipperFOnRight =
	nonMonadic applyZipperFOnRightM

-- |monadic version of 'applyZipperFOnAllChildren'
applyZipperFOnAllChildrenM :: Monad m => (Zipper a -> m (Node a)) -> Zipper a -> m (Zipper a)
applyZipperFOnAllChildrenM f zip =
	case ((down 0 zip) ) of
		Nothing -> return $ zip
		Just firstChild -> return . fromJust . up =<< applyZipperFOnRightM f firstChild

-- |monadic version of 'applyZipperFOnRight'
applyZipperFOnRightM :: Monad m => (Zipper a -> m (Node a)) -> Zipper a -> m (Zipper a)
applyZipperFOnRightM f zipper = do
	newChild <- changeFocusM f zipper
	case (right $ newChild) of
		Nothing -> return $ newChild
		Just rightNeighbour -> applyZipperFOnRightM f rightNeighbour

delFromIndex index list = take index list ++ (tail $ drop index list)
addFromIndex index val list = take index list ++ [val] ++ drop index list

-- |a special kind of mapping over a tree, where the function sees a nodes context as well (a zipper is a subtree plus its context)
mapSeesZipper :: (Zipper a -> b) -> Zipper a -> Tree b
mapSeesZipper = nonMonadic mapSeesZipperM

-- |monadic version of 'mapZipperF'
mapSeesZipperM :: Monad m => (Zipper a -> m b) -> Zipper a -> m (Tree b)
mapSeesZipperM f zipper = do
	val <- f zipper
	children <- mapM (mapSeesZipperM f) $ allDown zipper
	return $ node val children

{-
testTree = node 0 [ node 1 [ leaf 1.0, leaf 1.1 ], node 2 [ leaf 2.0 ], node 3 []]
simpleTree = node 0 [ leaf 1, leaf 2, leaf 3]
-}
