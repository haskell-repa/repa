
module Data.Vector.Repa
        ( Vector
        , Map(..)
        , Zip(..)
        , vzip3,        vzipWith3
        , vzip4,        vzipWith4
        , vlength
        , vreplicate
        , vreplicates
        , vcompute
        , vchain
        , vunchainP
        , vunchainS)
where
import Data.Vector.Repa.Operators.Zip
import Data.Vector.Repa.Operators.Replicate
import Data.Vector.Repa.Repr.Chain
import Data.Vector.Repa.Base
import Data.Array.Repa.Eval


vcompute  = suspendedComputeP
{-# INLINE [4] vcompute #-}

vunchainP = unchainP
{-# INLINE [4] vunchainP #-}

vunchainS = unchainS
{-# INLINE [4] vunchainS #-}

vchain  = chain
{-# INLINE [4] vchain #-}

