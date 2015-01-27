
module Data.Repa.Array.Internals.Flat
        ( Repr   (..)
        , Flat   (..), Bulk1
        , Vector)
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk


-- | Class of array representations.
class Repr r where
 -- | Proxy for an array representation. The representations are singletons, 
 --   so there is only one value of a given type. 
 --   Use with an explicit type signature, like @(repr :: B)@.
 repr    :: r


data Flat r ex sh 
        = Flat r (RowWise ex sh)

type Bulk1 r a
        = Bulk  (Flat r Safe DIM1) a

type Vector r a
        = Array (Flat r Safe DIM1) a


instance Layout (RowWise ex sh)
      => Layout (Flat r ex sh) where

        type Index (Flat r ex sh)
                = Index (RowWise ex sh)

        extent    (Flat _ rw)     = extent rw
        {-# INLINE extent #-}

        toIndex   (Flat _ rw) ix  = toIndex rw ix
        {-# INLINE toIndex #-}

        fromIndex (Flat _ rw) n   = fromIndex rw n
        {-# INLINE fromIndex #-}

