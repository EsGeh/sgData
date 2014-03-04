{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module SGData.Tuple where

import SGData.Classes

import SGCard


-- tuples as index
instance MultiIndex N1 Int Int
instance MultiIndex N2 (Int,Int) Int

-- "single element tuple"
instance ListCT Int Int N1 where
	get n x = x
instance FromListCT Int Int N1 where
	fromListCT [x] = x
instance ToListCT Int Int N1 where
	toListCT i = [i]


-- isomorphic to lists
--instance ToListCT a a N1 where toListCT = id
instance ToListCT (a,a) a N2 where toListCT (x,y) = [x,y]
instance ToListCT (a,a,a) a N3 where toListCT (x,y,z) = [x,y,z]
instance FromListCT (a,a) a N2 where fromListCT [x,y] = (x,y)
instance FromListCT (a,a,a) a N3 where fromListCT [x,y,z] = (x,y,z)
instance ListCT (a,a) a N2 where get n l = toListCT l !! fromCard n
instance ListCT (a,a,a) a N3 where get n l = toListCT l !! fromCard n
