--{-# LANGUAGE TypeSynonymInstances #-}
module Main where


import SGCard
import SGData.Tree
import SGData.TreeZipper
import qualified SGData.MatrixTS as Mat

import Data.Maybe(fromJust)

main = do
	putStrLn "------------------------------------"
	putStrLn "Matrix examples:"
	testMat
	putStrLn "------------------------------------"
	putStrLn "Tree examples:"
	testTree

testMat = do
	putStrLn "testMatrix:"
	print $ testMatrix
	let mat1 = fromJust testMatrix

	
	putStrLn $ "Mat.mGet (1,1) mat1:"
	print $ Mat.mGet (1,1) mat1

	putStrLn $ "Mat.mGetRow 1 mat1"
	print $ Mat.mGetRow 1 mat1

testTree = do
	let tree1 = unfoldTree unfoldF 3
	putStrLn "unfoldTree unfoldF 3:"
	print tree1
	
	putStrLn $ "mapZipperF attachIndex (zipperTop tree1):"
	print $ mapZipperF attachIndex (zipperTop tree1)

unfoldF :: Int -> (Int, [Int])
unfoldF depth = (depth, listSub)
	where
		listSub =
			if depth>0
			then take 3 (repeat (depth-1) )
			else []

attachIndex :: Zipper Int -> (Int, Int)
attachIndex zipper = (value $ focus zipper, index zipper)

type Matrix a = Mat.Matrix N3 N3 a

--m = Mat.m (n3, n3)
mFromListRow = Mat.mFromListRow (n3, n3)

testMatrix :: Maybe (Matrix Int)
testMatrix = mFromListRow [[1,2,3],[4,5,6],[7,8,9]]


instance (Show a, Card m, Card n) => Show (Mat.Matrix m n a) where
	show m = unlines $ map show $ Mat.mGetAllRows m
