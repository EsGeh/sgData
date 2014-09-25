{-# LANGUAGE ScopedTypeVariables #-}
module SGData.TreeZipper(
	-- * zipper
	Direction(..), Path,
	Zipper(), 
	-- ** construct a zipper
	zipper, zipperTop,
	-- ** getters
	fromZipper,
	focus, context, depth, index,
	-- * Tree Context
	TreeContext,
	-- * zipper setters
	walkPath,moveZipper, moveIfPossible,
	pathFromZipper,
	top, leftBottom,
	up, down, left, right,
	allDown,
	-- ** apply functions depening on nodes, depending on their context in the tree
	unfoldWithZipper,
	unfoldWithZipperM,
	applyZipperFOnFocusRec,
	applyZipperFOnFocus,
	applyZipperFOnAllChildren,
	applyZipperFOnRight,

	mapZipperF,
) where

import SGData.Tree
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Identity

data Direction = Up | Down Int
type Path = [Direction]

data TreeContext a = TCTop | TCDown {
	childIndex :: Int,
	parentCxt :: TreeContext a,
	parentTree :: Tree a
} deriving(Show)
mapToChildIndex f context = context{ childIndex = f (childIndex context) }
mapToParentCxt f context = context{ parentCxt = f (parentCxt context) }
mapToParentTree f context = context{ parentTree = f (parentTree context) }

-- | a zipper is a node plus some information about its context (its position in the tree)
data Zipper a = Zipper { fromZipper :: (Node a, TreeContext a) } deriving(Show)
mapToFocus f zipper =
	let (focus, context) = fromZipper zipper
	in Zipper (f focus, context)
mapToContext f zipper =
	let (focus, context) = fromZipper zipper
	in Zipper (focus, f context)
-- | retrieve the node from a zipper
focus = fst . fromZipper 
-- | retrieve the context from a zipper
context = snd . fromZipper
depth z = case (up z) of
	Nothing -> 0
	Just parent -> depth parent + 1
index z = case context z of
	TCTop -> 0
	_ -> childIndex (context z)

{-
mapZipperF :: (Tree a-> Tree a) -> Zipper a -> Maybe (Zipper a)
mapZipperF f zipper = (top zipper)
-}

walkPath :: Path -> Zipper a -> Maybe (Zipper a)
walkPath list zipper = case list of
	[] -> Just zipper
	dir:restDirections -> (moveZipper dir zipper) >>= (walkPath restDirections)

moveIfPossible dir zipper = maybe zipper id (moveZipper dir zipper)

moveZipper :: Direction -> Zipper a -> Maybe (Zipper a)
moveZipper dir zipper = case dir of
	Up -> up zipper
	Down index -> down index zipper

pathFromZipper :: Zipper a -> Path
pathFromZipper zipper = case context zipper of
	TCTop -> []
	TCDown { childIndex = i } ->
		(pathFromZipper (fromJust $ up zipper)) ++ [Down i]

zipper = Zipper 
zipperTop tree = Zipper (tree, TCTop)
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
down int zipper = do
	child <- safeIndexing int (children $ focus zipper)
	let parentTree = applyOnChildren (delFromIndex int) $ focus zipper
	return $ Zipper ( 
		child,
		TCDown {
			childIndex = int,
			parentCxt = context zipper,
			parentTree = parentTree })

down' :: Int -> Zipper a -> Maybe (Zipper a)
down' int zipper = case children (focus zipper) of
	[] -> Nothing
	children -> if int < length children && int >= 0
		then Just $ Zipper $ (
			children !! int,
			TCDown {
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
up zipper = case context zipper of
	TCTop -> Nothing
	context -> Just $ Zipper $ (upperNode, parentCxt context)
		where
			upperNode = applyOnChildren (addFromIndex (childIndex context) (focus zipper)) (parentTree context)
left zipper = case context zipper of
	TCTop -> Nothing
	context -> up zipper >>= down (childIndex context - 1)
right zipper = case context zipper of
	TCTop -> Nothing
	context -> up zipper >>= down (childIndex context + 1)

--upRec z = maybe z up $ upRec z
asLongAsPossible :: Direction -> Zipper a -> Zipper a
asLongAsPossible dir z = maybe z (asLongAsPossible dir) $ moveZipper dir z 



{- |> unfoldWithZipper f startVal

creates a tree using f, whereas the result may depend on branches which have been calculated already.
Branches are created top down from left to right.
If they have not, the value of the zipper will be Nothing.
-}
unfoldWithZipper :: forall param a . (param -> Zipper (Maybe a) -> (a,[param])) -> param -> Tree a
unfoldWithZipper f params =
	runIdentity $ unfoldWithZipperM (\p -> return . f p) params

-- | monadic version of 'unfoldWithZipper'
unfoldWithZipperM :: Monad m => (param -> Zipper (Maybe a) -> m (a,[param])) -> param -> m (Tree a)
unfoldWithZipperM f params =
	return . fmap fromJust 
	=<<
		(unfoldWithZipperM' f params $
		(zipperTop $ blankInfiniteTree))
	where
		blankInfiniteTree = unfoldTree (const (Nothing, repeat Nothing)) Nothing

unfoldWithZipperM' :: forall m param a . Monad m => (param -> Zipper (Maybe a) -> m (a,[param])) -> param -> Zipper (Maybe a) -> m (Tree (Maybe a))
unfoldWithZipperM' f params zipper = do
	(val, subParams) <- f params zipper :: m (a, [param])
	listSubTrees <- foldM conc [] $ subParams
	return $ node (Just val) listSubTrees
	where
		conc :: [Tree (Maybe a)] -> param -> m [Tree (Maybe a)]
		conc list subParam = do
			nextTree <- unfoldWithZipperM' f subParam subZipper
			return $ list ++ [nextTree]
			where
				subZipper = 
					mapToContext calcCtxt $
					fromJust $
					down currentIndex $
					zipper
				calcCtxt :: TreeContext (Maybe a) -> TreeContext (Maybe a)
				calcCtxt TCTop = TCTop
				calcCtxt ctxt@TCDown{ parentTree = pT } =
					ctxt{ parentTree = newParentTree }
					where
						newParentTree :: Tree (Maybe a)
						newParentTree = flip applyOnChildren pT $ \children ->
							list ++ drop (length list) children :: [Node (Maybe a)]
				currentIndex = length list

{- |> applyZipperFOnFocusRec f zipper

recursively applies f to the whole subtree of the node the zipper points to
-}
applyZipperFOnFocusRec :: (Zipper a -> Node a) -> Zipper a -> Zipper a
applyZipperFOnFocusRec f zipper' = applyZipperFOnAllChildren (focus . applyZipperFOnFocusRec f) $ applyZipperFOnFocus f zipper'

{-
-- |monadic version of 'applyZipperFOnFocusRec'
applyZipperFOnFocusRecM :: Monad m => (Zipper a -> m (Node a)) -> Zipper a -> m (Zipper a)
applyZipperFOnFocusRecM f zipper' = undefined
-}

{- |> applyZipperFOnFocus f zipper

returns a new Zipper, where f has been applied to the node the zipper points to
-}
applyZipperFOnFocus :: (Zipper a -> Node a) -> Zipper a -> Zipper a
applyZipperFOnFocus f zipper' = zipper (newTree,context)
	where
		newTree = f zipper'
		(oldTree,context) = fromZipper zipper'

{- |> applyZipperFOnFocus f zipper

returns a new Zipper where f has been applied to all children
-}
applyZipperFOnAllChildren :: (Zipper a -> Node a) -> Zipper a -> Zipper a
applyZipperFOnAllChildren f zipper = case ((down 0 zipper) ) of
	Nothing -> zipper
	Just firstChild -> fromJust $ up $ applyZipperFOnRight f firstChild

-- | 
applyZipperFOnRight :: (Zipper a -> Node a) -> Zipper a -> Zipper a
applyZipperFOnRight f zipper = case (right $ newCurrent) of
	Nothing -> newCurrent
	Just rightNeighbour -> applyZipperFOnRight f rightNeighbour
	where
		newCurrent = applyZipperFOnFocus f zipper

delFromIndex index list = take index list ++ (tail $ drop index list)
addFromIndex index val list = take index list ++ [val] ++ drop index list


-- |a special kind of mapping over a tree, where the function sees a nodes context as well (a zipper is a subtree plus its context)
mapZipperF :: (Zipper a -> b) -> Zipper a -> Tree b
mapZipperF f zipper = node (f zipper) $ map (mapZipperF f) $ allDown zipper

testTree = node 0 [ node 1 [ leaf 1.0, leaf 1.1 ], node 2 [ leaf 2.0 ], node 3 []]
simpleTree = node 0 [ leaf 1, leaf 2, leaf 3]
