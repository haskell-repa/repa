
module Data.Array.Repa.Flow.Par.Fold
        ( folds
        , foldsSplit)
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
import System.IO.Unsafe

-------------------------------------------------------------------------------
-- | Fold Segmented.
--
--   Takes a flow of segment lengths and a flow of elements,
--   and reduces each segment to a value individually.
--
--   TODO: make distroOfSplitSegd handle unbalanced SplitSegds and remove
--   the requirement that the source blow is balanced.
--
--   BROKEN: this isn't finished.
--
folds   :: U.Unbox a   
        => (a -> a -> a) -> a
        -> Segd
        -> Flow rep BB a
        -> Flow rep BB a

folds f z segd ff
        = foldsSplit f z (Segd.splitSegd segd) ff
{-# INLINE folds #-}


-------------------------------------------------------------------------------
-- | Fold Segmented, with a pre-split segment descriptor.
--
--   BROKEN: this isn't finished.
--
foldsSplit 
        :: U.Unbox a
        => (a -> a -> a) -> a
        -> SplitSegd
        -> Flow rep BB a
        -> Flow rep BB a

foldsSplit f !z segd (Flow distro frag)
 = Flow distro' frag'
 where
        !frags          = distroBalancedFrags distro
        !(I# segs)      = U.length (Segd.lengths (Segd.splitOriginal segd))
        distro'         = balanced frags segs

        !chunks = Segd.splitChunks segd

        -- TODO: This won't work
        --       Parallel flows need their own state
        !mvars  = unsafePerformIO $ V.replicateM (I# chunks) newEmptyMVar

        frag' n
         = let  !chunk          = V.unsafeIndex (Segd.splitChunk segd) (I# n)
                !segLens        = Segd.chunkLengths chunk
                !(I# segsHere)  = U.length segLens

                getSegIxLen seg = (I# seg, U.unsafeIndex segLens (I# seg))
                fSegIxLens      = Seq.generate segsHere getSegIxLen 

                -- If the first segment is split across a thread boundary
                -- then we need to send the partial result to our
                -- left neighbour.
                mLeftVar
                 | n ># 0#
                 , Segd.chunkOffset chunk ># 0#
                 = Just (V.unsafeIndex mvars (I# (n -# 1#)))

                 | otherwise
                 = Nothing

                 -- If the last segment is split across a thread boundary
                 -- then we need to read the partial result from our 
                 -- right neighbour.
                mRightVar
                 | n <# (chunks -# 1#)
                 , chunkRight <- V.unsafeIndex (Segd.splitChunk segd) (I# (n +# 1#))
                 , Segd.chunkOffset chunkRight ># 0#
                 = Just (V.unsafeIndex mvars (I# n))

                 | otherwise
                 = Nothing

           in   foldsTradeSeq f z segsHere fSegIxLens (frag n) 
                        mLeftVar
                        mRightVar
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
        -> Int#                   -- ^ Total number of segments to fold here.
                                  --   This is one more than the maxium segment index.
        -> Seq.Flow r1 (Int, Int) -- ^ Flow of segment indices and their lengths.
        -> Seq.Flow r2 a          -- ^ Flow of input data.
        -> Maybe (MVar a)         -- ^ Write result of first segment to this MVar.
        -> Maybe (MVar a)         -- ^ Combine result of last segment with this MVar.
        -> Seq.Flow r2 a          -- ^ Flow of fold results.

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


        -- Pull the length of the first segment.
        get1' (!stateA, !stateB) push1
         =  getLen1 stateA $ \r
         -> case r of 
                Seq.Yield1 (I# segIx, I# len) _ 
                         -> foldSeg stateB push1 segIx len
                Seq.Done -> push1 Seq.Done
        {-# INLINE get1' #-}


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
        foldSeg stateB push1 !segIx !len
         = go8 len z
         where  
                go8 n  !acc
                 -- If there are less than a whole packet of elements
                 -- reamaining in the segment them switch to pulling
                 -- them one at a time.
                 |  n <# 8#
                 =  go1 n acc

                 -- There are at least eight elements remaining, 
                 -- so we can do an unrolled fold.
                 |  otherwise
                 =  getElem8 stateB  $ \r
                 -> case r of
                        Seq.Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                         -> let !acc' = acc `f` x0 `f` x1 `f` x2 `f` x3 
                                            `f` x4 `f` x5 `f` x6 `f` x7
                            in  go8 (n -# 8#) acc'

                        Seq.Pull1
                         -> go1 n acc

                -- We've reached the end of the segment.
                --  Combine with our left and right neighbours if needed,
                --  and push the final result to the consumer.
                --  Set the hint to False because we're not going to give it
                --  eight result elements next time either.
                go1 0# !acc
                 = do   
                        -- If the right-var is set then we need to combine
                        -- the partial result with the result from our
                        -- right neighbour.
                        acc'    <- if segIx ==# segIxMax
                                        then recvRight acc
                                        else return acc

                        -- If the left-var is set then we send our result
                        -- to our right neighbour instead.
                        sendLeft acc'


                -- Folding elements one at a time.
                --   Note that we don't need to switch back to go8 here, 
                --   that happend automatically at the end of each segment.
                go1 n  !acc
                 =  getElem1 stateB $ \r
                 -> case r of
                        Seq.Yield1 x1 _
                         -> let !acc' = acc `f` x1
                            in   go1 (n -# 1#) acc'

                        Seq.Done
                         -> go1 0# acc 
                         -- error "folds: not enough elements for segment."
                         --  If we hit this case then the segment lengths 
                         --  don't match the data we have, but just return the
                         --  current acc to avoid producing code for the error.
                         --  (yeah, you heard me)


                recvRight acc
                 | Just vRight  <- mRightVar
                 = do   xRight  <- readMVar vRight
                        return  $ f acc xRight

                 | otherwise
                 =      return acc
                {-# INLINE recvRight #-}


                sendLeft acc
                 | segIx ==# 0#
                 , Just vLeft   <- mLeftVar
                 = do   putMVar vLeft acc
                        push1 $ Seq.Done

                 | otherwise
                 =      push1 $ Seq.Yield1 acc False
                {-# INLINE sendLeft #-}

        {-# INLINE foldSeg #-}
{-# INLINE [1] foldsTradeSeq #-}



