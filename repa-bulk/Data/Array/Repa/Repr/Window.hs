
module Data.Array.Repa.Repr.Window
        ( W
        , Array  (..)
        , Window (..)
        , windowed
        , entire)
where
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Shape


-- Windows ----------------------------------------------------------------------------------------
data W r

instance Bulk r sh a 
      => Bulk (W r) sh a where

 data Array (W r) sh a
        = WArray
        { warrayStart   :: sh
        , warrayShape   :: sh
        , warrayBuffer  :: Array r sh a }

 extent (WArray _ sh _) = sh
 {-# INLINE [1] extent #-}

 index  (WArray start sh buffer) ix
        | not $ inShapeRange zeroDim sh ix
        = error "repa-bulk.index[W]: index out of bounds"

        | otherwise
        = index buffer (addDim start ix)
 {-# INLINE [1] index #-}


-- | Wrap a window around an exiting array.
windowed :: Shape sh => sh -> sh -> Array r sh a -> Array (W r) sh a
windowed start shape arr
        = WArray start shape arr
{-# INLINE [1] windowed #-}


-- | Wrap a window 
entire :: Bulk r sh a => Array r sh a -> Array (W r) sh a
entire arr
        = WArray zeroDim (extent arr) arr
{-# INLINE [1] entire #-}


---------------------------------------------------------------------------------------------------

-- | Class of array representations that can be windowed directly.
--   The underlying representation can encode the window.
class Shape sh => Window r sh where
 window :: sh -> sh -> Array r sh a -> Array r sh a

instance Shape sh => Window (W r) sh where
 window start _shape (WArray wStart wShape arr)
        = WArray (addDim wStart start) wShape arr
 {-# INLINE window #-}
