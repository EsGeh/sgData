module SGData.Point where

import SGData.MatrixTS
import SGCard



{-
-- pointtor operations:
infixl 8 $+$ -- pointtor addition
infixl 8 |-| -- pointtor subtraction
infixl 9 |*| -- component-by-component multiplication (!)
infixl 9 |/| -- component-by-component division (!)
infixl 9 *| -- scalar mult
infixl 9 |* -- scalar mult
infixl 9 /| -- scalar div
infixl 9 |/ -- scalar div
--(:+:) :: (Num a) => Point a -> Point a -> Point a
l |+| r = (pointX l + pointX r,  pointY l + pointY r)
l |-| r = (pointX l - pointX r,  pointY l - pointY r)
l |*| r = (pointX l * pointX r,  pointY l * pointY r)
l |/| r = (pointX l / pointX r,  pointY l / pointY r)
scalar *| point = (scalar * (pointX point), scalar * (pointY point))
(|*) = flip (*|)
scalar /| point = (scalar / (pointX point), scalar / (pointY point))
point |/ scalar = ((pointX point) / scalar, (pointY point) / scalar)

pointMap f (x,y) = (f x, f y)
-}
