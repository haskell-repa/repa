
module Data.Array.Repa.Chain
        ( Chain(..)
        , DistChain(..)
        , Distro (..)
        , Step (..)

          -- * Reduction
        , fold,         foldD
        , foldM,        foldMD

          -- * Evaluation
        , evalM,        evalMD

          -- * Constructors
        , chain,        chainD
        , replicate,    replicateD
        , replicateEach, replicateEachD
        , replicatesD

          -- * Maps and Zips
        , map,           mapD
        , mapIx,         mapIxD
        , unsafeZipLock, unsafeZipLockD

          -- * Indexed
        , indexed,      indexedD

          -- * Append
        , appendSegs

          -- * Unboxed vector interface.
        , chainUnboxed,   chainUnboxedD
        , unchainUnboxed, unchainUnboxedD)

where
import Data.Array.Repa.Chain.Base
import Data.Array.Repa.Chain.Eval
import Data.Array.Repa.Chain.Replicate
import Data.Array.Repa.Chain.Map
import Data.Array.Repa.Chain.Indexed
import Data.Array.Repa.Chain.Append
import Control.Monad.ST
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude
        hiding (replicate, map, zip, zipWith, zipWith3)


-- | Convert an unboxed vector to a chain.
chainUnboxed :: U.Unbox a => U.Vector a -> Chain a
chainUnboxed vec
 = let  !(I# len)  = U.length vec
        get ix     = vec `U.unsafeIndex` (I# ix)
   in   chain len get
{-# INLINE [1] chainUnboxed #-}


-- | Convert an unboxed vector to a distributed chain.
chainUnboxedD :: U.Unbox a => Distro -> U.Vector a -> DistChain a
chainUnboxedD distro vec
 = let  get ix     = vec `U.unsafeIndex` (I# ix)
   in   chainD distro get
{-# INLINE [1] chainUnboxedD #-}


-- | Evaluate a chain, returning the elements in a vector.
unchainUnboxed :: U.Unbox a => Chain a -> U.Vector a
unchainUnboxed c@(Chain len _ _)
 = runST 
 $ do   !mvec    <- UM.unsafeNew (I# len)

        let write ix x 
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalM write c

        U.unsafeFreeze mvec
{-# INLINE [1] unchainUnboxed #-}


-- | Evaluate a distributed chain, returning the elements in a vector.
unchainUnboxedD :: U.Unbox a => DistChain a -> U.Vector a
unchainUnboxedD dc@(DistChain distro _)
 = runST 
 $ do   !mvec    <- UM.unsafeNew (I# (distroLength distro))

        let write ix x 
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalMD write dc

        U.unsafeFreeze mvec
{-# INLINE [1] unchainUnboxedD #-}


