{-# LANGUAGE UndecidableInstances #-}

-- | Low level interface to parallel array filling operators.
module Data.Array.Repa.Eval
        ( Elt       (..)
        , Fillable  (..)
        , Fill      (..)
        , FillRange (..)
        
        -- * The Gang
        , Gang, seqGang, forkGang, gangSize, gangIO, gangST, theGang
        
        -- * Chunked filling
        , fillChunkedS
        , fillChunkedP

        -- * Blockwise filling
        , fillBlock2P
        
        -- * Cursored blockwise filling
        , fillCursoredBlock2S
        , fillCursoredBlock2P)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import System.IO.Unsafe

-- | Parallel computation of array elements.
instance (Repr r1 e, Fill r1 r2 sh e) => Load r1 r2 sh e where
 {-# INLINE load #-}
 load arr1
  = unsafePerformIO
  $ do  marr2    <- newMArr (size $ extent arr1) 
        fillP arr1 marr2
        unsafeFreezeMArr (extent arr1) marr2
      