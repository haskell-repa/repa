{-# LANGUAGE TypeOperators, RankNTypes, PatternGuards #-}

-- | Compute the Discrete Fourier Transform (DFT) along the low order dimension
--   of an array. 
--
--   This uses the naive algorithm and takes O(n^2) time. 
--   However, you can transform an array with an arbitray extent, unlike with FFT which requires
--   each dimension to be a power of two.
--
--   The `dft` and `idft` functions also compute the roots of unity needed.
--   If you need to transform several arrays with the same extent then it is faster to
--   compute the roots once using `calcRootsOfUnity` or `calcInverseRootsOfUnity`, 
--   then call `dftWithRoots` directly.
--
--   You can also compute single values of the transform using `dftWithRootsSingle`.
module Data.Array.Repa.Algorithms.DFT 
        ( dftP
        , idftP
        , dftWithRootsP
        , dftWithRootsSingleS)
where
import Data.Array.Repa.Algorithms.DFT.Roots     as R
import Data.Array.Repa.Algorithms.Complex       as R
import Data.Array.Repa                          as R
import Prelude                                  as P


-- | Compute the DFT along the low order dimension of an array.
dftP    :: (Shape sh, Monad m)
        => Array U (sh :. Int) Complex
        -> m (Array U (sh :. Int) Complex)

dftP v
 = do   rofu    <- calcRootsOfUnityP (extent v)
        dftWithRootsP rofu v
{-# INLINE dftP #-}


-- | Compute the inverse DFT along the low order dimension of an array.
idftP   :: (Shape sh, Monad m)
        => Array U (sh :. Int) Complex
        -> m (Array U (sh :. Int) Complex)

idftP v
 = do   let _ :. len    = extent v
        let scale       = (fromIntegral len, 0)
        rofu            <- calcInverseRootsOfUnityP (extent v)
        roots           <- dftWithRootsP rofu v
        computeP $ R.map (/ scale) roots
{-# INLINE idftP #-}


-- | Generic function for computation of forward or inverse DFT.
--      This function is also useful if you transform many arrays with the same extent, 
--      and don't want to recompute the roots for each one.
--      The extent of the given roots must match that of the input array, else `error`.
dftWithRootsP
        :: (Shape sh, Monad m)
        => Array U (sh :. Int) Complex          -- ^ Roots of unity.
        -> Array U (sh :. Int) Complex          -- ^ Input array.
        -> m (Array U (sh :. Int) Complex)

dftWithRootsP rofu arr
        | _ :. rLen     <- extent rofu
        , _ :. vLen     <- extent arr
        , rLen /= vLen
        = error $    "dftWithRoots: length of vector (" P.++ show vLen P.++ ")"
                P.++ " does not match the length of the roots (" P.++ show rLen P.++ ")"

        | otherwise
        = computeP $ R.traverse arr id (\_ k -> dftWithRootsSingleS rofu arr k)
{-# INLINE dftWithRootsP #-}            


-- | Compute a single value of the DFT.
--      The extent of the given roots must match that of the input array, else `error`.
dftWithRootsSingleS
        :: Shape sh
        => Array U (sh :. Int) Complex          -- ^ Roots of unity.
        -> Array U (sh :. Int) Complex          -- ^ Input array.
        -> (sh :. Int)                          -- ^ Index of the value we want.
        -> Complex

dftWithRootsSingleS rofu arrX (_ :. k)
        | _ :. rLen     <- extent rofu
        , _ :. vLen     <- extent arrX
        , rLen /= vLen
        = error $    "dftWithRootsSingle: length of vector (" P.++ show vLen P.++ ")"
                P.++ " does not match the length of the roots (" P.++ show rLen P.++ ")"

        | otherwise
        = let   sh@(_ :. len)   = extent arrX

                -- All the roots we need to multiply with.
                wroots          = fromFunction sh elemFn
                elemFn (sh' :. n) 
                        = rofu ! (sh' :. (k * n) `mod` len)

          in  R.sumAllS $ R.zipWith (*) arrX wroots
{-# INLINE dftWithRootsSingleS #-}


