
module Data.Repa.Array.Window
        ( W      (..)
        , Array  (..)
        , Window (..)
        , windowed
        , entire)
where
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Bulk
#include "repa-stream.h"


-- Windows ----------------------------------------------------------------------------------------
data W r = W r


-- | Windowed arrays.
instance Repr r => Repr (W r) where
 type Safe   (W r) = W r
 type Unsafe (W r) = W r
 repr = W repr
 {-# INLINE repr #-}
 

-- | Windowed arrays.
instance Bulk r sh a 
      => Bulk (W r) sh a where

 data Array (W r) sh a                  = WArray !sh !sh !(Array r sh a)
 extent (WArray _ sh _)                 = sh
 index  (WArray start _ buffer) ix      = index buffer (addDim start ix)
 safe   arr                             = arr
 unsafe arr                             = arr
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY index  #-}
 {-# INLINE_ARRAY safe   #-}
 {-# INLINE_ARRAY unsafe #-}


-- | Wrap a window around an exiting array.
windowed :: Shape sh => sh -> sh -> Array r sh a -> Array (W r) sh a
windowed start shape arr
        = WArray start shape arr
{-# INLINE_ARRAY windowed #-}


-- | Wrap a window around an existing array that encompases the entire array.
entire :: Bulk r sh a => Array r sh a -> Array (W r) sh a
entire arr
        = WArray zeroDim (extent arr) arr
{-# INLINE_ARRAY entire #-}


---------------------------------------------------------------------------------------------------
-- | Class of array representations that can be windowed directly.
--   The underlying representation can encode the window.
class Bulk r sh a => Window r sh a where
 window :: sh -> sh -> Array r sh a -> Array r sh a

instance Bulk r sh a => Window (W r) sh a where
 window start _shape (WArray wStart wShape arr)
        = WArray (addDim wStart start) wShape arr
 {-# INLINE_ARRAY window #-}

