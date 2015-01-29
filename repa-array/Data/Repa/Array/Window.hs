
module Data.Repa.Array.Window
        ( W          (..)
        , Array      (..)
        , Windowable (..)
        , windowed
        , entire)
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
#include "repa-array.h"


-- Windows --------------------------------------------------------------------
data W l = Window (Index l) (Index l) l

instance Layout l => Layout (W l) where
        type Index (W l) = Index l

        extent    (Window _ sz _)  
                = sz
        {-# INLINE extent #-}

        toIndex   (Window _st _sz inner) ix  
                = toIndex inner ix              -- TODO: wrong, use offsets
        {-# INLINE toIndex #-}

        fromIndex (Window _st _sz inner) ix     -- TODO: wrong, use offsets
                = fromIndex inner ix
        {-# INLINE fromIndex #-}


-- | Windowed arrays.
instance Bulk l a 
      => Bulk (W l) a where

 data Array (W l) a             = WArray !(Index l) !(Index l) !(Array l a)
 layout (WArray st  sz inner)   = Window st sz (layout inner)
 index  (WArray st _  inner) ix = index inner (addDim st ix)
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


-- | Wrap a window around an exiting array.
windowed :: Index l -> Index l -> Array l a -> Array (W l) a
windowed start shape arr
        = WArray start shape arr
{-# INLINE_ARRAY windowed #-}


-- | Wrap a window around an existing array that encompases the entire array.
entire :: Bulk l a => Array l a -> Array (W l) a
entire arr
        = WArray zeroDim (extent $ layout arr) arr
{-# INLINE_ARRAY entire #-}


-------------------------------------------------------------------------------
-- | Class of array representations that can be windowed directly.
--
--   The underlying representation can encode the window, 
--   without needing to add a wrapper to the existing layout.
--
class Bulk l a    => Windowable l a where
 window :: Index l -> Index l -> Array l a -> Array l a

instance Bulk l a => Windowable (W l) a where
 window start _shape (WArray wStart wShape arr)
        = WArray (addDim wStart start) wShape arr
 {-# INLINE_ARRAY window #-}


