
module Data.Array.Repa.Flow.Seq.Operator.Slurp
        (drain, slurp)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Seq.Source
import Data.Array.Repa.Flow.Seq.Sink
import Prelude                                          hiding (take)
import GHC.Exts


-- | Pull all elements from a source `Source` and push them into 
--   a `Sink`, returning the total number of elements. 
--
--   * This calls the `sinkEject` method on the sink afterwards.
drain :: Elt a
        => Source FD a  -- ^ Pull elements from this source.
        -> Sink   FD a  -- ^ Push elements into this sink.
        -> IO Int       -- ^ Total number of elements drained.

drain source sink
 = do   
        -- Start the source and get the maximum expected size.
        source'@(Source istate' getSize' _ _ _)   
                <- startSource source
        state   <- getSourceState istate'
        size    <- getSize' state

        -- Start the sink, giving it the maximum expected size.
        sink'  @(Sink (SinkStateActive cstate') eject' _ _)    
                <- startSink size sink

        -- Pull all elements from the flow and push them into the coflow.
        count   <- slurp Nothing source' sink'

        -- Signal to the sink that we've pushed all the elements.
        eject' cstate'

        return  count


-- | Pull elements from a `Source` and push them into a `Sink`, 
--
--   * Both source and sink must have already been started.
--   
--   * This does NOT call the `sinkEject` method afterwards.
--
slurp   :: Elt a
        => Maybe Int    -- ^ Maximum number of elements to slurp,
                        --   or `Nothing` to slurp all available.
        -> Source FS a  -- ^ Pull elements from this source.
        -> Sink   FS a  -- ^ Push elements into this sink.
        -> IO Int       -- ^ Total number of elements slurped.

slurp stop 
        (Source (SourceStateActive stateA) _ _ get1  get8)
        (Sink   (SinkStateActive   stateB) _   feed1 feed8)

 = do   let here = "seq.slurp"

        refCount <- inew 1
        iwrite here refCount 0# (-1#)

        let
         {-# INLINE slurpSome #-}
         slurpSome ix
          = do  slurp8 ix
                I# ix'     <- iread here refCount 0# 

                slurp1 ix'
                I# ix''    <- iread here refCount 0#

                case stop of
                 Just (I# limit)
                  -> if ix'' ==# ix || ix'' >=# limit
                        then return (I# ix'')
                        else slurpSome ix''

                 Nothing
                  -> if ix'' ==# ix
                        then return (I# ix'')
                        else slurpSome ix''

         {-# INLINE slurp1 #-}
         slurp1 ix 
          | Just (I# limit) <- stop
          , ix >=# limit
          =     iwrite here refCount 0# ix

          |  otherwise
          =  get1 stateA $ \r
          -> case r of
                Yield1 x switch
                 -> do  
                        feed1 stateB (Snack1 x)

                        -- Touch 'x' here because we don't want the code
                        -- that computes it to be floated into the switch
                        -- and then copied.
                        touch x

                        if switch 
                         then iwrite here refCount 0# (ix +# 1#)
                         else slurp1 (ix +# 1#)

                Done  
                 -> do  iwrite here refCount 0# ix
                        
         {-# INLINE slurp8 #-}
         slurp8 ix
          | Just (I# limit)     <- stop
          , ix +# 8# ># limit
          =     iwrite here refCount 0# ix

          | otherwise
          =  get8 stateA $ \r
          -> case r of
                Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                 -> do  feed8 stateB (Snack8 x0 x1 x2 x3 x4 x5 x6 x7)
                        slurp8 (ix +# 8#)

                Pull1   
                 ->     iwrite here refCount 0# ix

        slurpSome 0#
{-# INLINE [0] slurp #-}

