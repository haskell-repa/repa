
module Data.Array.Repa.Vector.Operators.Append
        ( -- * Uniform appends
          append

          -- * Segmented appends
        , appends
        , appendsWithResultSegd)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Segd
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Operators.Traverse
import Data.Array.Repa.Vector.Operators.Zip             as R
import qualified Data.Array.Repa.Vector.Segd            as Segd
import qualified Data.Array.Repa.Flow.Par               as F
import GHC.Exts


-- | Uniform append.
append
        :: (Bulk r1 a, Bulk r2 a, Shape sh)
        => Array r1 (sh :. Int) a
        -> Array r2 (sh :. Int) a
        -> Array D  (sh :. Int) a

append arr1 arr2
 = traverse2 arr1 arr2 fnExtent fnElem
 where
        (_ :. n)        = extent arr1

        fnExtent (sh :. i) (_  :. j)
                = sh :. (i + j)

        fnElem f1 f2 (sh :. i)
                | i < n         = f1 (sh :. i)
                | otherwise     = f2 (sh :. (i - n))
{-# INLINE [4] append #-}


-- | Segmented append.
appends :: (Bulk r1 a, Bulk r2 a)
        => Segd                 -- ^ Segment descriptor of the A array.
        -> Vector r1 a          -- ^ The A array.
        -> Segd                 -- ^ Segment descriptor of the B array.
        -> Vector r2 a          -- ^ The B array.
        -> Vector (O mode BB) a

appends segdA vecA segdB vecB
 = let  
        -- TODO: use bulk computeP function
        !lensResult     = unflowP
                        $ flow
                        $ R.zipWith (+) (Segd.lengths segdA)
                                        (Segd.lengths segdB)

        segdResult      = Segd.splitSegd 
                        $ Segd.fromLengths lensResult

   in   appendsWithResultSegd
                segdResult
                segdA vecA 
                segdB vecB
{-# INLINE [4] appends #-}


-- | Segmented append, 
--   taking a pre-split segment descriptor for the result array.
appendsWithResultSegd
        :: (Bulk r1 a, Bulk r2 a)
        => SplitSegd            -- ^ Segment descriptor of the result.
        -> Segd                 -- ^ Segment descriptor of the A array.
        -> Vector r1 a          -- ^ The A array.
        -> Segd                 -- ^ Segment descriptor of the B array.
        -> Vector r2 a          -- ^ The B array.
        -> Vector (O mode BB) a

appendsWithResultSegd segdr segdA vecA segdB vecB
 = AFlow (DistBB ex) ff
 where  ex      = Z :. (Segd.elements $ Segd.unsplitSegd segdr)
        getA i  = linearIndex vecA (I# i)
        getB i  = linearIndex vecB (I# i)
        ff      = F.appends
                        segdr 
                        segdA getA
                        segdB getB
{-# INLINE [4] appendsWithResultSegd #-}

