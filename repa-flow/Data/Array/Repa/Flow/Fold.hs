
module Data.Array.Repa.Flow.Fold
        ( foldl
        , folds
        , sums)
where
import Data.Array.Repa.Flow.Base
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude hiding (foldl)
import System.IO.Unsafe


-------------------------------------------------------------------------------
-- | Fold Left. Reduce a flow to a single value.
foldl :: U.Unbox a => (a -> b -> a) -> a -> Flow b -> a
foldl f z !(Flow start _ get1 get8)
 = unsafePerformIO
 $ do   
        outRef  <- UM.unsafeNew 1
        UM.unsafeWrite outRef 0 z

        state  <- start

        let 
         eat1 !acc
          = get1 state $ \r 
          -> case r of
                Yield1 x1 hint
                 -> let !acc' = (acc `f` x1)
                    in  if hint then eat8 acc'
                                else eat1 acc'

                Done
                 -> UM.write outRef 0 acc

         eat8 !acc 
          =  get8 state $ \r 
          -> case r of
                Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                  -> eat8 ( acc `f` x0 `f` x1 `f` x2 `f` x3
                                `f` x4 `f` x5 `f` x6 `f` x7) 

                Pull1
                  -> eat1 acc

        eat8 z
        UM.read outRef 0
{-# INLINE [1] foldl #-}


-------------------------------------------------------------------------------
-- | Fold Segmented. Takes a flow of segment lengths and a flow of elements,
--   and reduces each segment to a value individually.
folds :: U.Unbox a => (a -> b -> a) -> a -> Flow Int -> Flow b -> Flow a
folds f !z (Flow startA  sizeA getLen1  _) 
           (Flow startB _sizeB getElem1 getElem8)
 = Flow start' size' get1' get8'
 where
        start'
         = do   stateA  <- startA
                stateB  <- startB
                return  (stateA, stateB)

        size' (stateA, _stateB)
         =      sizeA stateA

        -- Pull the length of the first segment.
        get1' (!stateA, !stateB) push1
         =  getLen1 stateA $ \r
         -> case r of 
                Yield1 (I# len) _ -> foldSeg stateB push1 len
                Done              -> push1 Done
        {-# INLINE get1' #-}

        -- Producing eight copies of the loop that folds a segment won't make
        -- anything faster, so tell the consumer they can only pull one result
        -- at a time.
        get8' _ push8
         = push8 $ Pull1
        {-# INLINE get8' #-}

        -- Fold a single segment
        --  Start out pulling four elements at a time, then switch to
        --  one-at-a-time when we get close to the end of the segment.
        --  We don't want to pull any elements from the _next_ segment
        --  because then we'd have to stash them until the next result
        --  was pulled from us.
        foldSeg stateB push1 !len
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
                        Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                         -> let !acc' = acc `f` x0 `f` x1 `f` x2 `f` x3 
                                            `f` x4 `f` x5 `f` x6 `f` x7
                            in  go8 (n -# 8#) acc'

                        Pull1
                         -> go1 n acc

                -- We've reached the end of the segment.
                -- Push the final result to the consumer.
                --  Set the hint to False because we're not going to give it
                --  eight result elements next time either.
                go1 0# !acc
                 = push1 $ Yield1 acc False

                -- Folding elements one at a time.
                --   Note that we don't need to switch back to go8 here, 
                --   that happend automatically at the end of each segment.
                go1 n  !acc
                 =  getElem1 stateB $ \r
                 -> case r of
                        Yield1 x1 _
                         -> let !acc' = acc `f` x1
                            in   go1 (n -# 1#) acc'

                        Done
                         -> go1 0# acc 
                         -- error "folds: not enough elements for segment."
                         --  If we hit this case then the segment lengths 
                         --  don't match the data we have, but just return the
                         --  current acc to avoid producing code for the error.
                         --  (yeah, you heard me)
        {-# INLINE foldSeg #-}
{-# INLINE [1] folds #-}


-- | Sum Segmented. Takes a flow of segment lenghths and a flow of elements,
--   and sums the elements of each segment individually.
sums :: (U.Unbox a, Num a) => Flow Int -> Flow a -> Flow a
sums ffLens ffElems
        = folds (+) 0 ffLens ffElems
{-# INLINE [1] sums #-}
