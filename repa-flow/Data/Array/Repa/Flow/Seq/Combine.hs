
module Data.Array.Repa.Flow.Seq.Combine
        (combine2)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as Report


-- | Combine two flows, using a tag stream to tell us which of the data
--   streams to take the next element from.
--  
-- @
-- combine2 [F T T F F T] [1 2 3] [4 5 6]
--  = [1 4 5 2 3 6]
-- @
--
combine2 
        :: Flow mode Bool 
        -> Flow mode a 
        -> Flow mode a 
        -> Flow mode a

combine2 (Flow startF sizeF reportF getF1 _)
         (Flow startA sizeA reportA getA1 _)
         (Flow startB sizeB reportB getB1 _)
 = Flow start' size' report' get1' get8'
 where
        start'
         = do   stateF  <- startF
                stateA  <- startA
                stateB  <- startB
                return  (stateF, stateA, stateB)

        size'   (stateF, stateA, stateB)
         = do   szF   <- sizeF stateF
                szA   <- sizeA stateA
                szB   <- sizeB stateB
                return  $ sizeMin szF (sizeMin szA szB)

        report' (stateF, stateA, stateB)
         = do   repF    <- reportF stateF
                repA    <- reportA stateA
                repB    <- reportB stateB
                return  $ Report.Combine repF repA repB

        get1'   (stateF, stateA, stateB) push1
         =  getF1 stateF $ \mxF
         -> case mxF of
                Yield1 f _
                 -> case f of
                     True    -> getA1 stateA $ \mfA
                             -> case mfA of
                                 Yield1 a _     -> out push1 (Just a)
                                 Done           -> out push1 Nothing

                     False   -> getB1 stateB $ \mfB
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
