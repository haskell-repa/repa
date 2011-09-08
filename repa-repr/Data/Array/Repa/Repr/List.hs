
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


instance Repr L a where
 {-# INLINE index #-}
 index  (AList sh lst) ix
        = lst !! toIndex sh ix

 {-# INLINE extent #-}
 extent (AList sh _)
        = sh


instance Load L L e where
 {-# INLINE load #-}
 load arr = arr


instance Load D L e where
 {-# INLINE load #-}
 load (Delayed sh getElem)
        = AList sh
        $ [getElem (fromIndex sh ix) 
                | ix <- [0 .. size sh - 1]]


-- | O(1). Wrap a list into an array.
fromList :: Shape sh => sh -> [e] -> Array L sh e
{-# INLINE fromList #-}
fromList sh xs = AList sh xs


-- | Convert an array to a flat list.
toList :: Shape sh => Array L sh e -> [e]
{-# INLINE toList #-}
toList arr
 = case load arr of
        AList _ xx      -> xx

