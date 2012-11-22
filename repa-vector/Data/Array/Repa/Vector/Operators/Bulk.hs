
module Data.Array.Repa.Vector.Operators.Bulk
        ( Bulk (..)
        , length)
where
import Data.Array.Repa.Vector.Base
import Prelude                          hiding (length)


-- | Bulk representations support random-access indexing and
--   their extent is known up-front.
class Bulk r a where
 -- | Shape-polymorphic indexing.
 index          :: Shape sh => Array r sh a -> sh -> a
 index arr ix   =  arr `linearIndex` toIndex (extent arr) ix
 
 -- | Linear indexing into the underlying row-major representation.
 linearIndex    :: Shape sh => Array r sh a -> Int -> a

 -- | Yield the extent of an array.
 extent :: Array r sh a -> sh


-- | Yield the length of a bulk vector.
length :: Bulk r a => Vector r a -> Int
length = size . extent
{-# INLINE [4] length #-}
