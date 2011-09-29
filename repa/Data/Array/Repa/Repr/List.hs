
module Data.Array.Repa.Repr.List
        ( L, Array (..)
        , fromList
        , toList)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed


-- | List arrays are represented as Haskell cons-lists.
--   Loading them does not invoke parallel computation because the structure
--   is naturally sequential.
data L
data instance Array L sh e
        = AList sh [e]

deriving instance (Show sh, Show e)
        => Show (Array L sh e)


-- | O(n) Indexing into a list.
instance Repr L a where
 {-# INLINE index #-}
 index  (AList sh lst) ix
        = lst !! toIndex sh ix

 {-# INLINE linearIndex #-}
 linearIndex (AList _ lst) ix
        = lst !! ix

 {-# INLINE extent #-}
 extent (AList sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AList sh xx) y
  = sh `deepSeq` (xx `deepSeqList` y)
  where deepSeqList xx' y'
         = case xx' of
                []      -> y'
                x : xs  -> x `seq` deepSeqList xs `seq` y'

-- | no-op.
instance Shape sh => Load L L sh e where
 {-# INLINE load #-}
 load arr = arr

-- | Sequential computation of list elements.
instance Shape sh => Load D L sh e where
 {-# INLINE load #-}
 load (ADelayed sh getElem)
        = AList sh
        $ [getElem (fromIndex sh ix) 
                | ix <- [0 .. size sh - 1]]


-- | O(1). Wrap a list into an array.
fromList :: Shape sh => sh -> [e] -> Array L sh e
{-# INLINE fromList #-}
fromList sh xs = AList sh xs


-- | Convert an array to a flat list.
-- 
toList :: Shape sh => Array L sh e -> [e]
{-# INLINE toList #-}
toList arr
 = case load arr of
        AList _ xx      -> xx

