{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, Rank2Types, FunctionalDependencies, ScopedTypeVariables #-}
module SGData.Card2 where

import Data.Reflection
import Data.Proxy

data Zero = Zero
data Succ a = Succ a

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5

n0 = Zero
n1 = Succ n0
n2 = Succ n1
n3 = Succ n2
n4 = Succ n3
n5 = Succ n4
n6 = Succ n5

instance Card Zero where fromCard _ = 0
instance (Card n) => Card (Succ n) where fromCard _ = fromCard (undefined :: n) + 1
instance Show Zero where
	show = show . fromCard
instance (Card n) => Show (Succ n) where
	show = show . fromCard


instance Container Int Zero where fromContainer _ = 0
instance (Container Int n) => Container Int (Succ n) where fromContainer _ = fromContainer (undefined :: n) + 1

class LE l r
instance LE Zero Zero
instance (LE l r) => LE (Succ l) (Succ r)
instance (LE l r) => LE l (Succ r)

class LT l r 
instance LT Zero N1
instance (LT l r) => LT (Succ l) (Succ r)
instance (LT l r) => LT l (Succ r)

--data CardProx config = CardProx

--class Index i where


class Container t c | c -> t where
	fromContainer :: c -> t

--
instance (Container t l, Container t r) => Container (t,t) (l,r) where
	fromContainer tuple = (fromContainer $ fst tuple, fromContainer $ snd tuple)

class Card c where
	fromCard :: c -> Int

class Card2 c where
	fromCard2 :: c -> (Int,Int)
class Card3 c where
	fromCard3 :: c -> (Int,Int,Int)

-- create a Card2 from a tuple of Cards:
instance (Card l, Card r) => Card2 (l,r) where
	fromCard2 (l,r) = (fromCard l, fromCard r)
{-
class (Card size) => CardList size l where
	getCard :: (LT index size, Card n) => index -> l -> n
class CardN c n | c -> n where
	fromCardN :: (Card index, Card res) => index -> c -> res
-}

-- data Proxy config = Proxy
instance (Reifies config Int) => Card (Proxy config) where
	fromCard x = (reflect x)
instance (Reifies config (Int,Int)) => Card2 (Proxy config) where
	fromCard2 x = (reflect x)
instance (Reifies config (Int,Int,Int)) => Card3 (Proxy config) where
	fromCard3 x = (reflect x)

{-
withCard :: Int -> (forall n. Card n => n -> w) -> w
withCard i f = withInt f'
	where
		f' x = 
-}

withInt :: Int -> (forall n . Reifies n Int => Proxy n -> w) -> w
withInt i f = reify i f

withInt2 :: (Int,Int) -> (forall n . Reifies n (Int,Int) => Proxy n -> w) -> w
withInt2 ii f = reify ii f

withInt3 :: (Int,Int,Int) -> (forall n . Reifies n (Int,Int,Int) => Proxy n -> w) -> w
withInt3 i f = reify i f

test = withInt 10 (show . fromCard)
test2 = withInt2 (1,2) $ show . fromCard2

bla1 :: forall bounds . Container Int bounds => bounds -> String
bla1 _ = show $ fromContainer (undefined :: bounds)
bla2 :: forall bounds . Container (Int,Int) bounds => bounds -> (Int,Int)
bla2 _ = fromContainer (undefined :: bounds)

{-
data CardInstance = forall config . Card config => CardInstance config
instance Card CardInstance where
	toInt (CardInstance config) = toInt config
-}
