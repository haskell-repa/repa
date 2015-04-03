{-# LANGUAGE TypeOperators, RankNTypes #-}

-- | Calculation of roots of unity for the forward and inverse DFT\/FFT.
module Data.Array.Repa.Algorithms.DFT.Roots
        ( calcRootsOfUnityP
        , calcInverseRootsOfUnityP)
where
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Complex


-- | Calculate roots of unity for the forward transform.
calcRootsOfUnityP
        :: (Shape sh, Monad m)
        => (sh :. Int)                  -- ^ Length of lowest dimension of result.
        -> m (Array U (sh :. Int) Complex)

calcRootsOfUnityP sh@(_ :. n) 
 = computeP $ fromFunction sh f
 where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) 
        = ( cos  (2 * pi * (fromIntegral i) / len)
          , - sin  (2 * pi * (fromIntegral i) / len))

    len = fromIntegral n


-- | Calculate roots of unity for the inverse transform.
calcInverseRootsOfUnityP
        :: (Shape sh, Monad m)
        => (sh :. Int)                  -- ^ Length of lowest dimension of result.
        -> m (Array U (sh :. Int) Complex)

calcInverseRootsOfUnityP sh@(_ :. n) 
 = computeP $ fromFunction sh f
 where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) 
        = ( cos  (2 * pi * (fromIntegral i) / len)
          , sin  (2 * pi * (fromIntegral i) / len))

    len = fromIntegral n
