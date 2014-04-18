module SGData.Vector2D where

-- use tuples like 2 dimensional vectors
type Vec a = (a,a)
vecX = fst
vecY = snd

-- vector operations:
infixl 8 |+| -- vector addition
infixl 8 |-| -- vector subtraction
infixl 9 |*| -- component-by-component multiplication (!)
infixl 9 |/| -- component-by-component division (!)
infixl 9 *| -- scalar mult
infixl 9 |* -- scalar mult
infixl 9 /| -- scalar div
infixl 9 |/ -- scalar div
--(:+:) :: (Num a) => Vec a -> Vec a -> Vec a
l |+| r = (vecX l + vecX r,  vecY l + vecY r)
l |-| r = (vecX l - vecX r,  vecY l - vecY r)
l |*| r = (vecX l * vecX r,  vecY l * vecY r)
l |/| r = (vecX l / vecX r,  vecY l / vecY r)
scalar *| vec = (scalar * (vecX vec), scalar * (vecY vec))
(|*) = flip (*|)
scalar /| vec = (scalar / (vecX vec), scalar / (vecY vec))
vec |/ scalar = ((vecX vec) / scalar, (vecY vec) / scalar)

vecMap f (x,y) = (f x, f y)
