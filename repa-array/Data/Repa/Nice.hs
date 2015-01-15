{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Nice
        (Nicer (..))
where
import Data.Repa.Array  as A
import Control.Monad
import Data.Word
import Prelude          as P


-- | Convert some value to a nice form.
--
--   In particular, nested Arrays are converted to nested lists, 
--   so that they are easier to work with on the ghci console.
-- 
--   As ghci automatically pretty prints lists, using @nice@ is more
--   fun than trying to @show@ the raw Repa array representations.
--
class Nicer a where
 type Nice a 
 nice :: a -> Nice a

instance Nicer Int where
 type Nice Int          = Int
 nice x = x

instance Nicer Float where
 type Nice Float        = Float
 nice x = x

instance Nicer Double where
 type Nice Double       = Double
 nice x = x

instance Nicer Char where
 type Nice Char         = Char
 nice x = x

instance Nicer Word8 where
 type Nice Word8        = Word8
 nice x = x

instance Nicer Word16 where
 type Nice Word16       = Word16
 nice x = x

instance Nicer Word32 where
 type Nice Word32       = Word32
 nice x = x

instance Nicer Word64 where
 type Nice Word64       = Word64
 nice x = x

instance Nicer a => Nicer (Maybe a) where
 type Nice (Maybe a)    = Maybe (Nice a)
 nice x = liftM nice x

instance (Nicer a, Nicer b) => Nicer (a, b) where
 type Nice (a, b)       = (Nice a, Nice b)
 nice (x, y)            = (nice x, nice y)

instance (Nicer a) => Nicer [a] where
 type Nice [a]          = [Nice a]
 nice xs                = P.map nice xs

instance (Bulk r DIM1 a, Nicer a) 
      => Nicer (A.Vector r a) where
 type Nice (Vector r a) = [Nice a]
 nice vec               = P.map nice $ toList vec







