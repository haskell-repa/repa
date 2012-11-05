
-- | Flows provide an incremental version of array fusion that allows the
--   the computation to be suspended and resumed at a later time.
module Data.Array.Repa.Flow
        ( FD, FS
        , Flow (..)
        , Step1(..)
        , Step8(..)

        -- * Conversion
        , flow
        , unflow
        , take
        , drain

        -- * Construction
        , generate
        , replicate
        , replicatesUnboxed
        , replicatesDirect
        , enumFromN

        -- * Combinators
        , map
        , zip,          zipLeft
        , zipWith,      zipLeftWith

        , pack
        , packByTag
        , filter
        , gather

        -- * Reduction
        , foldl
        , folds
        , sums)

where
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Generate
import Data.Array.Repa.Flow.Map
import Data.Array.Repa.Flow.Filter
import Data.Array.Repa.Flow.Fold
import GHC.Exts
import qualified Data.Array.Repa.Flow.Report    as R
import qualified Data.Vector.Unboxed            as U
import Prelude  hiding (map, zip, zipWith, foldl, filter, replicate, take)



-------------------------------------------------------------------------------
-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather :: U.Unbox a => U.Vector a -> Flow r Int -> Flow r a
gather !vec (Flow start size report get1 get8)
 = Flow start size report' get1' get8'
 where
        report' state
         = do   r       <- report state
                return  $ R.Gather r
        {-# NOINLINE report' #-}

        get1' !state push1
         =  get1 state $ \r
         -> case r of
                Yield1 ix hint
                 -> push1 $ Yield1 (U.unsafeIndex vec ix) hint

                Done
                 -> push1 $ Done
        {-# INLINE get1' #-}

        get8' state push8
         =  get8 state $ \r
         -> case r of
                Yield8 ix0 ix1 ix2 ix3 ix4 ix5 ix6 ix7
                 -> push8 $ Yield8      (U.unsafeIndex vec ix0)
                                        (U.unsafeIndex vec ix1)
                                        (U.unsafeIndex vec ix2)
                                        (U.unsafeIndex vec ix3)
                                        (U.unsafeIndex vec ix4)
                                        (U.unsafeIndex vec ix5)
                                        (U.unsafeIndex vec ix6)
                                        (U.unsafeIndex vec ix7)

                Pull1
                 -> push8 $ Pull1
        {-# INLINE get8' #-}

{-# INLINE [1] gather #-}


-- Other operators ------------------------------------------------------------
--
-- Do we need this in addition to zip?
--  link2   :: Flow a 
--         -> (Flow a -> Flow b)
--         -> (Flow a -> Flow c)
--         -> (Flow (b, c))

-- Unzip is dup2 followed by map snd / map fst
--  unzip2  :: Flow (a, b)
--          -> (Flow a, Flow b)

