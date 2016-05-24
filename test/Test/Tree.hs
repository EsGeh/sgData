{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Tree (
	htf_thisModulesTests
) where

import Test.Framework

import SGData.Tree
import SGData.TreeZipper

import qualified Data.Foldable as Fold


prop_fold val subValues =
	(Fold.foldr (+) 0 $
	node val (map leaf subValues))
	== 
	val + sum subValues
	where
		_ = val :: Int
		_ = subValues :: [Int]
