{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module SGData.Tuple where

import SGData.Classes

import SGCard


-- tuples as index
instance MultiIndex N0 N0 Int Int
--instance MultiIndex N1 N1 Int Int
instance MultiIndex N0 N1 (Int,Int) Int

instance BoundedCT Int Int N0 N0
instance BoundedCT (a,a) Int N0 N1
instance BoundedCT (a,a,a) Int N0 N2

-- "single element tuple"
instance ListCT Int Int N0 N0 where
	get n x = x
instance FromListCT Int Int N0 N0 where
	fromListCT [x] = x
instance ToListCT Int Int N0 N0 where
	toListCT i = [i]


-- isomorphic to lists
--instance ToListCT a a N1 where toListCT = id
instance ToListCT (a,a) a N0 N1 where toListCT (x,y) = [x,y]
instance ToListCT (a,a,a) a N0 N2 where toListCT (x,y,z) = [x,y,z]
instance FromListCT (a,a) a N0 N1 where fromListCT [x,y] = (x,y)
instance FromListCT (a,a,a) a N0 N2 where fromListCT [x,y,z] = (x,y,z)
instance ListCT (a,a) a N0 N1 where get n l = toListCT l !! fromCard n
instance ListCT (a,a,a) a N0 N2 where get n l = toListCT l !! fromCard n
