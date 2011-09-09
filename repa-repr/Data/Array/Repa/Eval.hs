
-- | Low level interface to parallel array filling operators.
module Data.Array.Repa.Eval
        (Elt(..)
        
        -- * The Gang
        , Gang, seqGang, forkGang, gangSize, gangIO, gangST, theGang
        
        -- * Chunked filling
        , fillChunkedS
        , fillChunkedP
        
        -- * Cursored blockwise filling
        , fillCursoredBlock2S
        , fillCursoredBlock2P)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Cursored
