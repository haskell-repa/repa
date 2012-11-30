
module Data.Array.Repa.Flow.Par.Append
        (appends)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Flow.Par.Segd                    (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par.Segd          as Segd
import qualified Data.Array.Repa.Flow.Seq.Append        as Seq
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector                            as V
import GHC.Exts


-- | Segmented append.
appends :: SplitSegd    -- ^ Segment descriptor of the result.
        -> Segd         -- ^ Segment descriptor of the A array.
        -> (Int# -> a)  -- ^ Get data from the A array.
        -> Segd         -- ^ Segment descriptor of the B array.
        -> (Int# -> a)  -- ^ Get data from the B array.
        -> Flow mode BB a

appends segdr segdA getElemA segdB getElemB 
 = Flow gang distro start frag
 where
        !gang    = Segd.splitGang segdr
        !distro  = Segd.distroOfSplitSegd segdr

        start 
         = return ()

        unbox (I# i)    = i
        getSegLenA i    = unbox (U.unsafeIndex (Segd.lengths segdA) (I# i))
        getSegIxA  i    = unbox (U.unsafeIndex (Segd.indices segdA) (I# i))
        getSegLenB i    = unbox (U.unsafeIndex (Segd.lengths segdB) (I# i))
        getSegIxB  i    = unbox (U.unsafeIndex (Segd.indices segdB) (I# i))

        frag _state n
         = let  !chunk   = V.unsafeIndex (Segd.splitChunk segdr) (I# n)
           in   Seq.appends
                        getSegLenA getSegIxA getElemA
                        getSegLenB getSegIxB getElemB
                        (Segd.chunkElems  chunk)
                        (Segd.chunkStart  chunk)
                        (Segd.chunkOffset chunk)
{-# INLINE [2] appends #-}



