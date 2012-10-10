
module Data.Array.Repa.Stream
        ( -- * Linear Stream
          Stream        (..)
        , DistStream    (..)
        , Distro        (..)
        , Step          (..)
        , Size          (..)

          -- * Reduction
        , fold,         foldD
        , foldM,        foldMD

          -- * Evaluation
        , evalM,        evalMD

          -- * Constructors
        , stream,       streamD
        , stream'
        , streamOfChain
        , streamOfChainD

        -- * Packing
        , pack,         packD

        -- * Unboxed vector interface
        , streamUnboxed,        streamUnboxedD
        , unstreamUnboxed,      unstreamUnboxedD)
where
import Data.Array.Repa.Stream.Base
import Data.Array.Repa.Stream.Eval
import Data.Array.Repa.Stream.Pack
import Data.Array.Repa.Distro
import Control.Monad.ST
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM


-- | Convert an unboxed vector to a stream.
streamUnboxed :: U.Unbox a => U.Vector a -> Stream a
streamUnboxed vec
 = let  !(I# len)       = U.length vec
        get ix          = vec `U.unsafeIndex` (I# ix)
   in   stream len get
{-# INLINE [1] streamUnboxed #-}


-- | Convert an unboxed vector to a distributed stream.
streamUnboxedD :: U.Unbox a => Distro -> U.Vector a -> DistStream a
streamUnboxedD distro vec
 = let  get ix          = vec `U.unsafeIndex` (I# ix)
   in   streamD distro get
{-# INLINE [1] streamUnboxedD #-}


-- | Evaluate a stream, returning the elements in an unboxed vector.
unstreamUnboxed :: U.Unbox a => Stream a -> U.Vector a
unstreamUnboxed s@(Stream size _ _)
 = case size of
        Exact len       -> unstreamUnboxed_exact len    s
        Max   lenMax    -> unstreamUnboxed_max   lenMax s
        Unknown         -> error "unstreamUnboxed: unknown streams not finished"

unstreamUnboxed_exact len s
 = runST
 $ do   !mvec   <- UM.unsafeNew (I# len)

        let write ix x
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalM write s
        U.unsafeFreeze mvec
{-# INLINE unstreamUnboxed_exact #-}

unstreamUnboxed_max lenMax s
 = runST
 $ do   !mvec   <- UM.unsafeNew (I# lenMax)

        let write ix x
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        len     <- evalM write s
        vec     <- U.unsafeFreeze mvec
        return  $ U.slice len (I# lenMax) vec
{-# INLINE unstreamUnboxed_max #-}


-- | Evalaute a distributed stream, returning the elements in an unboxed vector.
unstreamUnboxedD :: U.Unbox a => DistStream a -> U.Vector a
unstreamUnboxedD (DistStream _ frags frag)
 = let  chunk i = unstreamUnboxed (frag i)
        chunks  = map (\(I# i) -> chunk i) [0.. (I# (frags -# 1#))]
   in   U.concat chunks
{-# INLINE [1] unstreamUnboxedD #-}




