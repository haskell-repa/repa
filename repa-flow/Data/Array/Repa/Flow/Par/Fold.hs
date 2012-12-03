
module Data.Array.Repa.Flow.Par.Fold
        ( folds
        , foldsSplit
        , sums)
where
import GHC.Exts
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Flow.Par.Segd                    (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par.Segd          as Segd
import qualified Data.Array.Repa.Flow.Seq               as Seq
import qualified Data.Array.Repa.Flow.Seq.Report        as SeqReport
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector                            as V
import Control.Concurrent.MVar
import Control.Monad
import Prelude hiding (foldl)

here = "Data.Array.Repa.Flow.Par.Fold"


-------------------------------------------------------------------------------
-- | Segmented fold. Takes a flow of segment lengths and a flow of elements,
--   and reduces each segment to a value individually.
---
--   TODO: make distroOfSplitSegd handle unbalanced SplitSegds and remove
--   the requirement that the source blow is balanced.
--
folds   :: U.Unbox a
        => (a -> a -> a) -> a
        -> Segd
        -> Flow mode BB a
        -> Flow mode BN a

folds f z segd ff
        = foldsSplit f z (Segd.splitSegd (flowGang ff) segd) ff
{-# INLINE folds #-}


-------------------------------------------------------------------------------
-- | Segmented fold, with a pre-split segment descriptor.
--
foldsSplit 
        :: U.Unbox a
        => (a -> a -> a) -> a
        -> SplitSegd
        -> Flow mode BB a
        -> Flow mode BN a

foldsSplit f !z segd (Flow gang distro start frag)
 = Flow gang distro' start' frag'
 where
        !frags          = distroBalancedFrags distro

        -- TODO: When we produce the distribution of the result we know 
        -- up front which results will be produced on each thread, 
        -- but that distribution may itself be unbalanced.
        --  Here the element data is balanced, but the segment lengths are not.
        --  Want a third sort of 'Distro' for this.
        distro'         = unbalanced frags
        
        !chunks         = Segd.splitChunks segd

        start'
         = do   -- Initialise the source state.
                state1  <- start

                -- Create MVars so that neighbouring threads can communicate
                !mvars  <- V.replicateM (I# chunks) newEmptyMVar
                return  (state1, mvars)


        frag' (state1, mvars) n
         = let  !chunk          = vindex (Segd.splitChunk segd) (I# n)
                !segLens        = Segd.chunkLengths chunk
                !(I# segsHere)  = U.length segLens

                getSegIxLen seg = (I# seg, uindex here segLens (I# seg))
                fSegIxLens      = Seq.generate segsHere getSegIxLen 

                -- If the first segment is split across a thread boundary
                -- then we need to send the partial result to our
                -- left neighbour.
                mLeftVar        = getLeftVar mvars n

                 -- If the last segment is split across a thread boundary
                 -- then we need to read the partial result from our 
                 -- right neighbour.
                mRightVar       = getRightVar mvars n

           in   foldsTradeSeq f z segsHere fSegIxLens (frag state1 n) 
                        mLeftVar
                        mRightVar


        getLeftVar mvars n
                | !chunk  <- (V.!) (Segd.splitChunk segd) (I# n)
                , n ># 0#
                , Segd.chunkOffset chunk ># 0#
                = Just ((V.!) mvars (I# (n -# 1#)))

                | otherwise
                = Nothing
        {-# NOINLINE getLeftVar #-}
        --  NOINLINE because we don't want the branch in the core code.


        getRightVar mvars n
                | n <# (chunks -# 1#)
                , chunkRight <- (V.!) (Segd.splitChunk segd) (I# (n +# 1#))
                , Segd.chunkOffset chunkRight ># 0#
                = Just ((V.!) mvars (I# n))

                | otherwise
                = Nothing
        {-# NOINLINE getRightVar #-}
        --  NOINLINE because we don't want the branch in the core code.

{-# INLINE foldsSplit #-}


-------------------------------------------------------------------------------
-- | Fold Segmented with neighbour Trade.
--- 
--   Takes a flow of segment lengths and a flow of elements,
--   and reduces each segment to a value individually.
--
--   The result of folding the first segment is written into the left 
--   MVar, the result of folding the last segment is combined with 
--   the result read from the right MVar.
--
foldsTradeSeq   
        :: U.Unbox a
        => (a -> a -> a) -> a 
        -> Int#                     -- ^ Total number of segments to fold here.
                                    --   This is one more than the maxium segment index.
        -> Seq.Flow mode (Int, Int) -- ^ Flow of segment indices and their lengths.
        -> Seq.Flow mode a          -- ^ Flow of input data.
        -> Maybe (MVar a)           -- ^ Write result of first segment to this MVar.
        -> Maybe (MVar a)           -- ^ Combine result of last segment with this MVar.
        -> Seq.Flow mode a          -- ^ Flow of fold results.

foldsTradeSeq 
        f !z 
        !segsTotal
        (Seq.Flow startA  sizeA reportA getLen1  _) 
        (Seq.Flow startB _sizeB reportB getElem1 getElem8)
        !mLeftVar
        !mRightVar
 = Seq.Flow start' size' report' get1' get8'
 where
        -- The index of the maximum segment that we'll process.
        !segIxMax       = segsTotal -# 1#

        -- Get the starting state.
        start'
         = do   stateA  <- startA
                stateB  <- startB
                return  (stateA, stateB)


        -- Report the number of elements that will be produced.
        size' (stateA, _stateB)
         =      sizeA stateA


        -- Report internal state of the flow.
        report' (stateA, stateB)
         = do   rA      <- reportA stateA
                rB      <- reportB stateB
                return  $ SeqReport.Folds rA rB
        {-# NOINLINE report' #-}
        --  NOINLINE because it's not performance sensitive, 
        --  and we want to keep the intermediate code small.

        -- Producing eight copies of the loop that folds a segment won't make
        -- anything faster, so tell the consumer they can only pull one result
        -- at a time.
        get8' _ push8
         = push8 $ Seq.Pull1
        {-# INLINE get8' #-}


        -- Fold a single segment
        --  Start out pulling four elements at a time, then switch to
        --  one-at-a-time when we get close to the end of the segment.
        --  We don't want to pull any elements from the _next_ segment
        --  because then we'd have to stash them until the next result
        --  was pulled from us.
        get1' (!stateA, !stateB) push1
         = nextSeg 
         where  
                nextSeg
                 = getLen1 stateA $ \r
                 -> case r of
                        Seq.Yield1 (I# segIx, I# segLen) _
                         -> go8 segIx segLen z
 
                        Seq.Done 
                         -> push1 Seq.Done

                go8 segIx remaining !acc
                 -- If there are less than a whole packet of elements
                 -- reamaining in the segment them switch to pulling
                 -- them one at a time.
                 |  remaining <# 8#
                 =  go1 segIx remaining acc

                 -- There are at least eight elements remaining, 
                 -- so we can do an unrolled fold.
                 |  otherwise
                 =  getElem8 stateB  $ \r
                 -> case r of
                        Seq.Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                         -> let !acc' = acc `f` x0 `f` x1 `f` x2 `f` x3 
                                            `f` x4 `f` x5 `f` x6 `f` x7
                            in  go8 segIx (remaining -# 8#) acc'

                        Seq.Pull1
                         -> go1 segIx remaining acc

                -- We've reached the end of the segment.
                --  Combine with our left and right neighbours if needed,
                --  and push the final result to the consumer.
                --  Set the hint to False because we're not going to give it
                --  eight result elements next time either.
                go1 segIx 0# !acc
                 = do   
                        -- If the right-var is set then we need to combine
                        -- the partial result with the result from our
                        -- right neighbour.
                        acc'    <- if segIx ==# segIxMax
                                        then recvRight acc
                                        else return acc

                        -- If the left-var is set then we send our result
                        -- to our right neighbour instead.
                        result  <- sendLeft segIx acc'

                        case result of
                         Nothing        -> nextSeg
                         Just result'   -> push1 $ Seq.Yield1 result' False

                -- Folding elements one at a time.
                --   Note that we don't need to switch back to go8 here, 
                --   that happend automatically at the end of each segment.
                go1 segIx remaining !acc
                 =  getElem1 stateB $ \r
                 -> case r of
                        Seq.Yield1 x1 _
                         -> let !acc' = acc `f` x1
                            in   go1 segIx (remaining -# 1#) acc'

                        Seq.Done
                         -> go1 segIx 0# acc 
                         -- error "folds: not enough elements for segment."
                         --  If we hit this case then the segment lengths 
                         --  don't match the data we have, but just return the
                         --  current acc to avoid producing code for the error.
                         --  (yeah, you heard me)

                recvRight !acc
                 | Just vRight  <- mRightVar
                 = do   xRight  <- readMVar vRight
                        return  $ f acc xRight

                 | otherwise
                 =      return acc
                {-# NOINLINE recvRight #-}
                --  NOINLINE because we need to hide the branch to avoid
                --           code explosion in SMVM.

                sendLeft !segIx !acc
                 -- The elements we just folded belong to the last segment 
                 -- of our left neighbour. Send the result and fold a new segment
                 -- for ourselves.
                 | segIx ==# 0#
                 , Just vLeft   <- mLeftVar
                 = do   putMVar vLeft acc
                        return Nothing

                 | otherwise
                 =      return (Just acc)
                {-# NOINLINE sendLeft #-}
                --  NOINLINE because we need to hids to branch to avoid 
                --           code explosion in SMVM.

{-# INLINE [1] foldsTradeSeq #-}


-- | Segmented Sum. Takes a flow of segment lenghths and a flow of elements,
--   and sums the elements of each segment individually.
sums    :: (U.Unbox a, Num a) 
        => Segd 
        -> Flow mode BB a 
        -> Flow mode BN a

sums segd ffElems
        = folds (+) 0 segd ffElems
{-# INLINE [1] sums #-}


