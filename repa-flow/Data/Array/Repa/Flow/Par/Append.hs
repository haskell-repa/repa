
module Data.Array.Repa.Flow.Par.Append
        (appends)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Flow.Par.Segd                    (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par.Segd          as Segd
import qualified Data.Array.Repa.Flow.Seq.Append        as Seq
import GHC.Exts
import Debug.Trace

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
        here     = "par.appends"
        !gang    = Segd.splitGang segdr
        !distro  = Segd.distroOfSplitSegd segdr

        start 
         = do   putStrLn $ unlines
                        [ "appends[Par] start"
                        , "segdr    = " ++ show segdr
                        , "segdA    = " ++ show segdA
                        , "segdB    = " ++ show segdB ]
                return ()


        unbox (I# i)    = i
        getSegLenA i    = unbox (uindex (here ++ ".getSegLenA") (Segd.lengths segdA) (I# i))
        getSegIxA  i    = unbox (uindex (here ++ ".getSegIxA")  (Segd.indices segdA) (I# i))
        getSegLenB i    = unbox (uindex (here ++ ".getSegLenB") (Segd.lengths segdB) (I# i))
        getSegIxB  i    = unbox (uindex (here ++ ".getSegIxB")  (Segd.indices segdB) (I# i))

        frag _state n
         = let  !chunkR  = vindex here (Segd.splitChunks segdr) (I# n)
                !csegdR  = Segd.chunkSegd chunkR

           in   Seq.appends
                        getSegLenA getSegIxA getElemA
                        getSegLenB getSegIxB getElemB
                        (Segd.elements    csegdR)
                        (Segd.chunkStart  chunkR)
                        (Segd.chunkOffset chunkR)
{-# INLINE [2] appends #-}



