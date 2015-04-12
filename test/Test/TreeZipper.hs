{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.TreeZipper (
	htf_thisModulesTests
) where

import Test.Framework

import SGData.Tree
import SGData.TreeZipper

import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Foldable as Fold

prop_zipper tree =
	(focus $ zipperTop tree) == tree
	where
		_ = tree :: Tree Int

prop_unfoldZipper =
	unfoldSeesZipper unfoldFunc ()
	==
	(mapSeesZipper treePosition $ zipperTop $
		unfoldTree
			(\param -> ((), replicate (5 - param) (param+1) ))
			0
	)


test_unfoldZipper =
	putStrLn $ show $ unfoldSeesZipper unfoldFunc ()

-- test if it unfoldSeesZipper works with infinite trees:
prop_WithInfiniteTree =
	(value $ unfoldSeesZipper unfoldF ())
	== 0
	where
		unfoldF _ _ = (0, repeat ())

treePosition zipper = (depth zipper, index zipper)

unfoldFunc _ zipper =
	(treePosition zipper, replicate (5 - depth zipper) ())


instance (Arbitrary a) => Arbitrary (Node a) where
	arbitrary = sized tree'
		where
			tree' 0 = liftM leaf $ arbitrary
			tree' n =
				oneof [
					liftM leaf arbitrary,
					(liftM2 node) arbitrary $ replicateM n (tree' (n `div` 2))
					]
