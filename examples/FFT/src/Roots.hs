{-# LANGUAGE TypeOperators #-}

module Roots
	( calcRofu
	, calcInverseRofu)
where
import Data.Array.Repa
import StrictComplex


-- Roots of Unity ---------------------------------------------------------------------------------

-- | Fill a vector with roots of unity (Rofu)
calcRofu 
	:: Shape sh
	=> (sh :. Int) 			-- ^ Length of resulting vector.
	-> Array (sh :. Int) Complex

calcRofu sh@(_ :. n) 
 = force $ fromFunction sh f
 where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) =      (cos  (2 * pi * (fromIntegral i) / len))
		:*: (- sin  (2 * pi * (fromIntegral i) / len))

    len	= fromIntegral n


-- | Fill a vector with roots of unity (Rofu)
--	for the inverse transform.
calcInverseRofu
	:: Shape sh
	=> (sh :. Int) 			-- ^ Length of resulting vector.
	-> Array (sh :. Int) Complex

calcInverseRofu sh@(_ :. n) 
 = force $ fromFunction sh f
 where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) =      (cos  (2 * pi * (fromIntegral i) / len))
		:*:   (sin  (2 * pi * (fromIntegral i) / len))

    len	= fromIntegral n
