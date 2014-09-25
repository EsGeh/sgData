{-# LANGUAGE ScopedTypeVariables #-}
module SGData.Tree(
	-- * data types
	Tree,
	Node,
	-- * pseudo constructors
	leaf, node, unfoldTree, unfoldTreeM,
	-- * getters
	value,children,
	-- * setters
	-- ** manipulate the value of a node
	applyOnValue,
	-- ** manipulate the children of a node
	addChild, delChildFromIndex, mapOverChildren, applyOnChildren,
	-- * map functions over a tree:
	mapNodeF, mapNodeFM,
	-- * serializations
	--renderTree,
	--pShow

)
where

--import qualified Text.TextBlock as T
--import Text
import Data.Ratio
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

{-- node :: t -> [t] -> Node t
node value sublist = Node value (map node --}
-- | create a leaf from a value
leaf :: t -> Node t 
leaf value = Node value []

-- | apply function on the value of a node:
applyOnValue f n = node (f $ value n) (children n)

-- | create a node that has children
node :: t -> [Node t] -> Node t
node val list = Node val list

-- | create a tree from an unfold function
unfoldTree :: (a -> (t,[a])) -> a -> Tree t
unfoldTree f start = Node { value = v, children = map (unfoldTree f) c }
	where
		(v, c) = f start

-- | monadic version of 'unfoldTree'
unfoldTreeM :: Monad m => (param -> m (a, [param])) -> param -> m (Tree a)
unfoldTreeM f start = do
	(v, c) <- f start
	newChildren <- mapM (unfoldTreeM f) c
	return $ Node { value = v, children = newChildren }

addChild :: Node t -> Node t -> Node t
addChild child node = Node oldVal newChildren
	where
		oldVal = value node
		newChildren = child : (children node)
delChildFromIndex :: Node t -> Int -> Node t
delChildFromIndex node index = Node oldVal newChildren
	where
		oldVal = value node
		newChildren = take index (children node) ++ drop (index+1) (children node)

mapOverChildren :: (Node t -> Node t) -> Node t -> Node t
mapOverChildren f = applyOnChildren $ map f

applyOnChildren :: ([Node t] -> [Node t]) -> Node t -> Node t
applyOnChildren f node = Node oldVal newChildren
	where
		oldVal = value node
		newChildren = f $ children node


-- |this makes it possible to map over a tree:
instance Functor Node where
	fmap f (Node value []) = leaf (f value)
	fmap f (Node params (children)) = Node (f params) (map (fmap f) children)

-- |a special kind of mapping over a tree, where the function sees the whole subtree of each node
mapNodeF :: (Node a -> b) -> Tree a -> Tree b
mapNodeF f n = node (f n) (map (mapNodeF f) $ children n)

-- |monadic version of 'mapNodeF'
mapNodeFM :: Monad m => (Node a -> m b) -> Tree a -> m (Tree b)
mapNodeFM f n = do
	newValue <- f n
	newChildren <- mapM (mapNodeFM f) $ children n
	return $ node newValue newChildren

{--
instance Fold.Foldable Node where
	foldMap toMonoid (Node params list)= 
--}


testTree = node 0 [ leaf 1, leaf 2, leaf 3 ]
testTree2 = node 0 [ node 1 [leaf 1.1, leaf 1.2, leaf 1.3], leaf 2, leaf 3 ]
testTree3 = node 0 [ leaf 1 , node 2 [leaf 1.1, leaf 1.2, leaf 1.3 ], leaf 3 ]

{-
renderTree :: Depth -> RenderMethod t TextBlock -> RenderMethod (Tree t) TextBlock
renderTree maxDepth renderElement = if maxDepth <= 0
	then renderNothing
	else RenderMeth $ \size (Node params children) ->
		(runRenderMeth renderThis) size (params,children)
	where
		renderNothing = RenderMeth $ \size val -> m2empty
		--renderThis :: RenderMethod (t, [Tree t]) TextBlock
		renderThis = ud
			(div2ConstAndRest 1)
			renderElement
			renderChildren
			 
		--renderChildren :: RenderMethod [Tree t] TextBlock
		renderChildren = horizontal combPStd
			(repeat (renderTree (maxDepth-1) renderElement))
		--renderChildren = horizontal (repeat force)
-}


-- |this method should give a nice text serialisation of the tree:
{-pShow :: (Show t) => Int -> Width -> Tree t -> TextBlock 
pShow maxDepth width (Node params children) = if maxDepth <= 0
	then m2empty
	else
		(runRenderMeth $ divToLinesWE "..") (width,1) params === (runRenderMeth $ renderChildren) (width,10) (map (pShow (maxDepth-1) oneChildWidth) children)
	where
		oneChildWidth = floor $ fromIntegral width / fromIntegral (length children)
		renderChildren = horizontal (repeat force)-}

	
-- this method should give a nice text serialisation of the tree:
{-pShow width (Node params list) =
	(prettyFill width $ show params)
		++ (if length list > 0 then "\n" else "")
		++ subNodes
			where
				subNodes = if length list > 0 then unlines lines' else ""
				lines' = [ concat (map (getLine currentLine) subNodes') | currentLine <- [0..(deepestSubNode-1)]]
					 
				deepestSubNode = maximum $ map length subNodes'
				subNodes' = map (lines . pShow subWidth) list
				getLine n lines 
					| (n < length lines) = lines !! n
					| otherwise = ""
				prettyFill = Pretty.fill "[" "]" " " " " Pretty.MidJust
				subWidth = floor $ (width%1) / ((length list) %1)
				-}

-- |shows the tree in one line
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
