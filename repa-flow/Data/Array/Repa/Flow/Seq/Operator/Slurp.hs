
module Data.Array.Repa.Flow.Seq.Operator.Slurp
        (drain, slurp)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.CoFlow
import Prelude                                          hiding (take)
import GHC.Exts


-- | Fully evaluate a `Flow`\/`CoFlow` pair,
--   returning how many elements we got.
drain :: Elt a
        => Flow   FD a  -- ^ Pull elements from this flow.
        -> CoFlow FD a  -- ^ Push elements into this coflow.
        -> IO Int

drain fflow coflow
 = do   
        -- Start the flow and get the maximum expected size.
        flow'  @(Flow fstate' getSize' _ _ _)   
                <- startFlow fflow
        state   <- getFlowState fstate'
        size    <- getSize' state

        -- Start the coflow, giving it the maximum expected size.
        coflow'@(CoFlow (CoFlowStateActive cstate') eject' _ _)    
                <- startCoFlow size coflow

        -- Pull all elements from the flow and push them into the coflow.
        count   <- slurp Nothing flow' coflow'

        -- Signal to the coflow that we've pushed all the elements.
        eject' cstate'

        return  count


-- | Pull elements from a `Flow` and push them into a `CoFlow`.
--
--   Both flow and coflow must have already been started.
slurp   :: Elt a
        => Maybe Int    -- ^ Maximum number of elements to slurp.
        -> Flow   FS a  -- ^ Pull elements from this flow.
        -> CoFlow FS a  -- ^ Push elements into this coflow.
        -> IO Int       -- ^ Total number of elements slurped.

slurp stop 
        (Flow   (FlowStateActive   stateA) _ _ get1  get8)
        (CoFlow (CoFlowStateActive stateB) _   feed1 feed8)

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

