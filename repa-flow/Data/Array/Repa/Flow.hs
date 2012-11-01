-- Flows are stateful and incremental.
-- Taking a prefix only computes those elements.
-- 
-- Use dup2 for sharing, caches only as much data as required to 
-- handle the desync in the program.
--
module Data.Array.Repa.Flow
        ( Flow (..) 

        -- * Conversion
        , flow
        , unflow

        -- * Construction
        , generate
        , replicate
        , replicatesUnboxed
        , replicatesDirect
        , enumFromN

        -- * Pure combinators
        , map
        , zip
        , zipWith
        , pack
        , packInt
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
import qualified Data.Vector.Unboxed            as U
import Prelude  hiding (map, zip, zipWith, foldl, filter, replicate)


-- TODO: add a separate buffer function that converts the output
--       of pack back into a flow prepared to give four elements at a time.

-- TODO: add 'reflow', that evaluate elements into a buffer then 
--       converts back to a flow, for caching.

-- TODO: could write 'drop' function using an 'flowAdvance' field that
--       advances the flow without nessesarally computing the elements.

-- TODO: write 'take' to incrementally pull data.
--       take returns an unboxed vector of the given length
--       flow retuning take would just keep a second length
--       and push nothing after this length.

-- TODO: dup2 :: Flow a -> (Flow a, Flow a)
--       Creates two linked handles that buffer data until it has 
--       been pulled from both. Can still fuse into both consumers,
--       and only buffers the data that is required.
--       Doesn't force whole vector to be evaluated when we have sharing.
--       With higher degrees of duplication, might not to want to check
--       all consumers after every pull. Use a pull counter and only 
--       check for syncronisation after N pulls.

-- TODO: this should make it easy to write the parallel segmented
--       fold that exchanges data with its neighbours.

-- TODO: write recursive version of pack that buffers results.
--       until it gets enough to send a quad.

-- Think of situtions where we'll get desync between ends of dup2
-- Maybe with segmented fold, or pack / zip


-------------------------------------------------------------------------------
-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather :: U.Unbox a => U.Vector a -> Flow Int -> Flow a
gather !vec (Flow getSize get1 get8)
 = Flow getSize get1' get8'
 where
        get1' push1
         =  get1 $ \r
         -> case r of
                Yield1 ix hint
                 -> push1 $ Yield1 (U.unsafeIndex vec ix) hint

                Done
                 -> push1 $ Done
        {-# INLINE get1' #-}

        get8' push8
         =  get8 $ \r
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

