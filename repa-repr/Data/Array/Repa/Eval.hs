
-- | Low level interface to parallel array filling operators.
module Data.Array.Repa.Eval
        ( Elt  (..)
        , Fill (..)
        
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
import Data.Array.Repa.Repr.Delayed
import System.IO.Unsafe


instance Fill r e => Load D r e where
 {-# INLINE load #-}
 load (ADelayed sh getElem)
  = unsafePerformIO 
  $ do  marr    <- newMArr (size sh) 
        fillChunkedP (size sh) (writeMArr marr) (getElem . fromIndex sh)
        unsafeFreezeMArr sh marr 
