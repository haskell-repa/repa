
module Data.Array.Repa.Chain
        ( Chain(..)
        , DistChain(..)
        , Distro (..)
        , Step (..)

          -- * Reduction
        , fold,         foldD
        , foldM,        foldMD

          -- * Evaluation
        , evalM,         evalMD

          -- * Constructors
        , chain,         chainD
        , replicate,     replicateD
        , replicateEach, replicateEachD

          -- * Maps and Zips
        , map,           mapD
        , zipWith
        , zipWith3
        , zipWith4

          -- * Indexed
        , indexed,      indexedD

          -- * Unboxed vector interface
        , vchain,       vchainD
        , vunchain,     vunchainD)

where
import Data.Array.Repa.Chain.Base
import Data.Array.Repa.Chain.Eval
import Data.Array.Repa.Chain.Replicate
import Data.Array.Repa.Chain.Map
import Data.Array.Repa.Chain.Indexed
import Control.Monad.ST
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude
        hiding (replicate, map, zip, zipWith, zipWith3)


-- | Convert an unboxed vector to a chain.
vchain:: U.Unbox a => U.Vector a -> Chain a
vchain vec
 = let  !(I# len)  = U.length vec
        get ix     = vec `U.unsafeIndex` (I# ix)
   in   chain len get
{-# INLINE [1] vchain #-}


-- | Convert an unboxed vector to a distributed chain.
vchainD :: U.Unbox a => Distro -> U.Vector a -> DistChain a
vchainD distro vec
 = let  get ix     = vec `U.unsafeIndex` (I# ix)
   in   chainD distro get
{-# INLINE [1] vchainD #-}


-- | Evaluate a chain, returning the elements in a vector.
vunchain :: U.Unbox a => Chain a -> U.Vector a
vunchain c@(Chain len _ _)
 = runST 
 $ do   !mvec    <- UM.unsafeNew (I# len)

        let write ix x 
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalM write c

        U.unsafeFreeze mvec
{-# INLINE [1] vunchain #-}


-- | Evaluate a distributed chain, returning the elements in a vector.
vunchainD :: U.Unbox a => DistChain a -> U.Vector a
vunchainD dc@(DistChain distro _)
 = runST 
 $ do   !mvec    <- UM.unsafeNew (I# (distroLength distro))

        let write ix x 
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalMD write dc

        U.unsafeFreeze mvec
{-# INLINE [1] vunchainD #-}


