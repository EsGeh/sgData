{-# LANGUAGE ScopedTypeVariables #-}
module SGData.Tree(
	-- * data types
	Tree,
	Node,
	-- * apply functions to subelements
	changeValue, changeChildren,
	applyOnValue, applyOnChildren,
	-- ** monadic
	changeValueM, changeChildrenM,
	applyOnValueM, applyOnChildrenM,
	{-
	-- ** manipulate the children of a node
	addChild, delChildFromIndex, mapOverChildren, mapOverChildrenM,
	-}
	-- * create single nodes
	leaf, node,
	-- * create trees
	unfoldTree, unfoldTreeM,
	-- * getters
	value,children,
	-- * map functions over a tree
	mapSeesNode, mapSeesNodeM,
	updateNodes, updateNodesM,
	updateNodesTopDown, updateNodesTopDownM
)
where

--import qualified Text.TextBlock as T
--import Text
import SGData.Util
import Data.Ratio
import Data.Monoid
import Control.Monad
import qualified Data.Foldable as Fold


-- | a tree is a node
type Tree t = Node t
-- | a node
data Node t = Node {
	value :: t,
	children :: [Node t]
}
	deriving (Eq)


type Width = Int
type Depth = Int

------------------------------------------------------------------------------
-- * pseudo constructors
-- | create a leaf from a value
leaf :: a -> Node a
leaf value = Node value []

-- | create a node that has children
node :: a -> [Node a] -> Node a
node val list = Node val list

-- | create a tree from an unfold function
unfoldTree :: (param -> (a, [param])) -> param -> Tree a
unfoldTree =
	nonMonadic unfoldTreeM

-- | monadic version of 'unfoldTree'
unfoldTreeM :: Monad m => (param -> m (a, [param])) -> param -> m (Tree a)
unfoldTreeM f start = do
	(v, c) <- f start
	newChildren <- mapM (unfoldTreeM f) c
	return $ Node { value = v, children = newChildren }

------------------------------------------------------------------------------
-- * setters
-- ** manipulate a node

changeValue = 
	nonMonadic changeValueM

changeChildren = 
	nonMonadic changeChildrenM

changeValueM f n = do
	newVal <- f n
	return $ node newVal (children n)

changeChildrenM f n = do
	newC <- f n
	return $ node (value n) $ newC

-- | apply function on the value of a node:
applyOnValue =
	nonMonadic applyOnValueM

-- | apply function on the children of a node:
applyOnChildren :: ([Node a] -> [Node a]) -> Node a -> Node a
applyOnChildren =
	nonMonadic applyOnChildrenM

applyOnValueM f =
	changeValueM (f . value)

applyOnChildrenM :: Monad m => ([Node t] -> m [Node t]) -> Node t -> m (Node t)
applyOnChildrenM f =
	changeChildrenM (f . children)

------------------------------------------------------------------------------
-- ** manipulate the children of a node
addChild newChild = applyOnChildren (newChild:)
delChildFromIndex index = applyOnChildren $ \childs -> take index childs ++ drop (index+1) childs

-- |mapOverChildren f node = applyOnChildren (map f) node
mapOverChildren :: (Node a -> Node a) -> Node a -> Node a
mapOverChildren =
	nonMonadic mapOverChildrenM
	--applyOnChildren . map

mapOverChildrenM :: Monad m => (Node a -> m (Node a)) -> Node a -> m (Node a)
mapOverChildrenM = applyOnChildrenM . mapM

------------------------------------------------------------------------------
-- * map functions over a tree (bottom up is the default)
-- |a special kind of mapping over a tree, where the function sees the whole subtree of each node
mapSeesNode :: (Node a -> b) -> Tree a -> Tree b
mapSeesNode = 
	nonMonadic mapSeesNodeM

-- |monadic version of 'mapNodeF'
mapSeesNodeM :: Monad m => (Node a -> m b) -> Tree a -> m (Tree b)
mapSeesNodeM f n = do
	newValue <- f n
	newChildren <- mapM (mapSeesNodeM f) $ children n
	return $ node newValue newChildren

updateNodes :: (Node a -> Node a) -> Tree a -> Tree a
updateNodes =
	nonMonadic updateNodesM

updateNodesM :: Monad m => (Node a -> m (Node a)) -> Tree a -> m (Tree a)
updateNodesM f tree = do
	newTree <- mapOverChildrenM (updateNodesM f) tree
	f newTree

updateNodesTopDown :: (Node a -> Node a) -> Tree a -> Tree a
updateNodesTopDown =
	nonMonadic updateNodesTopDownM

updateNodesTopDownM :: Monad m => (Node a -> m (Node a)) -> Tree a -> m (Tree a)
updateNodesTopDownM f tree = do
	newTree <- f tree
	mapOverChildrenM (updateNodesTopDownM f) newTree

-- |this makes it possible to map over a tree:
instance Functor Node where
	fmap f (Node value []) = leaf (f value)
	fmap f (Node params (children)) = Node (f params) (map (fmap f) children)

instance Fold.Foldable Node where
	foldMap = foldMapTree

foldMapTree :: Monoid m => (a -> m) -> Node a -> m
foldMapTree toM n = toM (value n) `mappend` mconcat (map (foldMapTree toM) (children n))

testTree = node 0 [ leaf 1, leaf 2, leaf 3 ]
testTree2 = node 0 [ node 1 [leaf 1.1, leaf 1.2, leaf 1.3], leaf 2, leaf 3 ]
testTree3 = node 0 [ leaf 1 , node 2 [leaf 1.1, leaf 1.2, leaf 1.3 ], leaf 3 ]


-- |shows the tree nicely, using multiple lines
instance (Show t) => Show (Node t) where
	show = showTree 0
		where
			showTree tabCount (Node params children) = show params ++ "\n" ++ (foldl (++) "" $ map (tabs++) showChildren)
				where
					tabs = (take ((tabCount+1) * 2) $ cycle "| ") ++ "+-"
					--tabs = take ((tabCount+1) * 2) $ "+" ++ cycle "-"
					showChildren = case children of
						[] -> []
						_ -> map (showTree (tabCount+1)) children
			{-
			showTree tabCount (Node params children) = tabs ++ show params ++ showChildren ++ "\n"
				where
					tabs = take (tabCount * 2) $ cycle " "
					showChildren = case children of
						[] -> ""
						_ -> (foldl (++) " [\n" $ map (showTree (tabCount+1)) children) ++ tabs ++ "]"
			-}
