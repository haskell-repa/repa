
module Data.Array.Repa.Stream.Fold
        (foldSegs)
where
import Data.Array.Repa.Stream.Base
import Prelude  hiding (map, zipWith)


-- | Segmented Stream fold. Take segments from the given stream and fold each
--   using the supplied function and initial element. 
--
-- @
-- foldSegs (+) 0 [2, 3, 2] [10, 20, 30, 40, 50, 60, 70]
--  = [30,120,130]
-- @
--
foldSegs :: (a -> b -> a)        -- ^ Function to perform the fold.
         -> a                    -- ^ Initial element of each fold.
         -> Stream Int           -- ^ Stream of segment lengths.
         -> Stream b             -- ^ Stream of input data.
         -> Stream a             -- ^ Stream of fold results.
        
foldSegs f z (Stream sz ss0 nexts) (Stream _ vs0 nextv) 
 = Stream sz (Nothing, z, ss0, vs0) next
 where
        {-# INLINE next #-}
        next (Nothing,x,ss,vs) 
         = case nexts ss of
            Done         -> Done
            Update ss'   -> Update (Nothing,x, ss', vs)
            Yield  ss' n -> Update (Just n, z, ss', vs)

        next (Just 0,x,ss,vs) 
         = Yield (Nothing,z,ss,vs) x

        next (Just n,x,ss,vs) 
         = case nextv vs of
            Done         -> Done -- NEVER ENTERED (See Note)
            Update vs'   -> Update (Just n,x,ss,vs')
            Yield  vs' y -> let r = f x y
                            in  r `seq` (Update (Just (n-1), r, ss, vs'))
{-# INLINE foldSegs #-}


-- Note: [NEVER ENTERED]
-- ~~~~~~~~~~~~~~~~~~~~~
--  Cases marked NEVER ENTERED should be unreachable, assuming there are no 
--  bugs elsewhere in the library. We used to throw an error when these
--  branches were entered, but this was confusing the simplifier. It would be 
--  better if we could put the errors back, but we'll need to check that 
--  performance does not regress when we do so.
