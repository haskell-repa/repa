{-# LANGUAGE TypeOperators, RankNTypes #-}

-- | Calculation of roots of unity for the forward and inverse DFT\/FFT.
module Data.Array.Repa.Algorithms.DFT.Roots
	( calcRootsOfUnity
	, calcInverseRootsOfUnity)
where
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Complex

-- | Calculate roots of unity for the forward transform.
calcRootsOfUnity
	:: forall sh
	.  Shape sh
	=> (sh :. Int) 			-- ^ Length of lowest dimension of result.
	-> Array (sh :. Int) Complex

calcRootsOfUnity sh@(_ :. n) 
 = force $ fromFunction sh f
 where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) 
	= ( cos  (2 * pi * (fromIntegral i) / len)
	  , - sin  (2 * pi * (fromIntegral i) / len))

    len	= fromIntegral n


-- | Calculate roots of unity for the inverse transform.
calcInverseRootsOfUnity
	:: forall sh
	.  Shape sh
	=> (sh :. Int) 			-- ^ Length of lowest dimension of result.
	-> Array (sh :. Int) Complex

calcInverseRootsOfUnity sh@(_ :. n) 
 = force $ fromFunction sh f
 where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) 
	= ( cos  (2 * pi * (fromIntegral i) / len)
	  , sin  (2 * pi * (fromIntegral i) / len))

    len	= fromIntegral n
