
module Data.Array.Repa.Flow.Seq.Combine
        ( combine2
        , combines2)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as Report
import GHC.Exts

-- | Combine two flows, using a tag stream to tell us which of the data
--   streams to take the next element from.
--  
-- @
-- combine2 [F T T F F T] [1 2 3] [4 5 6]
--  = [1 4 5 2 3 6]
-- @
--
combine2 
        :: Flow mode Bool      -- ^ Flags vector.
        -> Flow mode a         -- ^ Elements of @A@ vector.
        -> Flow mode a         -- ^ Elements of @B@ vector.
        -> Flow mode a

combine2 (Flow startF sizeF  reportF getF1 _)
         (Flow startA _sizeA reportA getA1 _)
         (Flow startB _sizeB reportB getB1 _)
 = Flow start' size' report' get1' get8'
 where
        start'
         = do   stateF  <- startF
                stateA  <- startA
                stateB  <- startB
                return  (stateF, stateA, stateB)

        size'   (stateF, _stateA, _stateB)
         = do   szF     <- sizeF stateF
                return  szF

        report' (stateF, stateA, stateB)
         = do   repF    <- reportF stateF
                repA    <- reportA stateA
                repB    <- reportB stateB
                return  $ Report.Combine repF repA repB

        get1'   (stateF, stateA, stateB) push1
         -- Get the next flag.
         =  getF1 stateF $ \mxF
         -> case mxF of
                Yield1 f _
                 -> case f of
                     -- Emit an element from the first stream.
                     False    -> getA1 stateA $ \mfA
                             -> case mfA of
                                 Yield1 a _     -> out push1 (Just a)
                                 Done           -> out push1 Nothing

                     -- Emit an element from the second stream.
                     True   -> getB1 stateB $ \mfB
                             -> case mfB of
                                 Yield1 b _     -> out push1 (Just b)
                                 Done           -> out push1 Nothing

                Done -> out push1 Nothing

        out push1 mx
         = case mx of
                Nothing -> push1 Done
                Just x  -> push1 $ Yield1 x False

        get8' _ push1
         = push1 Pull1
{-# INLINE [4] combine2 #-}


-- | Segmented Stream combine. Like `combine2`, except that the flags select
--   entire segments of each data stream, instead of selecting one element
--   at a time.
--
-- @
-- combines2 
--      [F, F, T, F, T, T]
--      [2,1,3] [10,20,30,40,50,60]
--      [1,2,3] [11,22,33,44,55,66]
--  = [10,20,30,11,40,50,60,22,33,44,55,66]
-- @
--
--   This says take two elements from the first stream, then another one element 
--   from the first stream, then one element from the second stream, then three
--   elements from the first stream...
--
combines2
        :: Int#                 -- ^ Total length of result
        -> Flow mode Bool       -- ^ Flags.
        -> Flow mode Int        -- ^ Segment lengths of @A@ vector.
        -> Flow mode a          -- ^ Elements of @A@ vector.
        -> Flow mode Int        -- ^ Segment lengths of @B@ vector.
        -> Flow mode a          -- ^ Elements of @B@ vector.
        -> Flow mode a

combines2 resultLen
          (Flow startF     _sizeF     reportF     getF1     _)
          (Flow startLenA  _sizeLenA  reportLenA  getLenA1  _)
          (Flow startElemA _sizeElemA reportElemA getElemA1 _)
          (Flow startLenB  _sizeLenB  reportLenB  getLenB1  _)
          (Flow startElemB _sizeElemB reportElemB getElemB1 _)
 = Flow start' size' report' get1' get8'
 where
        sSource = 0#    -- Source vector currently being read.
        sRemain = 1#    -- Number of elements remaining in current segment.
                        -- Setting this to 0 force the first flag to be loaded
                        --  on the first call to get1.

        start'
         = do   stateF          <- startF
                stateLenA       <- startLenA
                stateElemA      <- startElemA
                stateLenB       <- startLenB
                stateElemB      <- startElemB

                state           <- inew 2
                iwrite state sSource 0#
                iwrite state sRemain 0#

                return  (state, stateF, stateLenA, stateElemA, stateLenB, stateElemB)


        size'   _
         =      return  $ Exact resultLen               -- TODO: this doesn't work incrementally.

        report' (!_, !stateF, !stateLenA, !stateElemA, !stateLenB, !stateElemB)
         = do   rpF             <- reportF     stateF
                rpLenA          <- reportLenA  stateLenA
                rpElemA         <- reportElemA stateElemA
                rpLenB          <- reportLenB  stateLenB
                rpElemB         <- reportElemB stateElemB
                return  $ Report.Combines rpF rpLenA rpElemA rpLenB rpElemB


        get1'   (!state, !stateF
                       , !stateLenA, !stateElemA
                       , !stateLenB, !stateElemB) push1
         = do   !(I# source)    <- iread state sSource 
                !(I# remain)    <- iread state sRemain
                next source remain

         where  next source remain
                 = if remain ==# 0# 
                        then nextSeg source remain
                        else fromSeg source remain

                -- Switch to the next segment.
                nextSeg source remain
                 =  getF1 stateF $ \mxF
                 -> case mxF of
                     Yield1 f _
                      -> case f of
                          False -> getLenA1 stateLenA $ \mLenA
                                -> case mLenA of
                                    Yield1 (I# lenA) _   -> next 0# lenA
                                    Done                 -> out source remain Nothing

                          True  -> getLenB1 stateLenB $ \mLenB
                                -> case mLenB of
                                    Yield1 (I# lenB) _   -> next 1# lenB
                                    Done                 -> out source remain Nothing

                     Done -> out source remain Nothing

                -- Emit an element from the given segment.
                fromSeg source remain
                 = case source of
                    0#  -> getElemA1 stateElemA $ \mElemA
                        -> case mElemA of
                            Yield1 elemA _  -> out source remain (Just elemA)
                            Done            -> out source remain Nothing

                    _   -> getElemB1 stateElemB $ \mElemB
                        -> case mElemB of
                            Yield1 elemB _  -> out source remain (Just elemB)
                            Done            -> out source remain Nothing

                out source remain mx
                 = case mx of
                    Nothing 
                     ->     push1 Done

                    Just x  
                     -> do  iwrite state sSource source
                            iwrite state sRemain (remain -# 1#)
                            push1 $ Yield1 x False

        get8' _ push1
         = push1 Pull1
