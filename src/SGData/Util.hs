module SGData.Util where

import Control.Monad.Identity

nonMonadic mapF f = runIdentity . mapF (return . f)

mapToFst f (a,b) = (f a, b)
mapToSnd f (a,b) = (a, f b)
