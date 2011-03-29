{-# LANGUAGE BangPatterns #-}

module Data.Array.Repa.Algorithms.Randomish
 	( randomishIntArray
	, randomishIntVector
	, randomishDoubleArray
	, randomishDoubleVector)
where
import Data.Word
import Data.Vector.Unboxed			(Vector)
import Data.Array.Repa				as R
import qualified Data.Vector.Unboxed.Mutable	as MV
import qualified Data.Vector.Unboxed		as V
import qualified Data.Vector.Generic		as G


-- | Use the ''minimal standard'' Lehmer generator to quickly generate some random
--   numbers with reasonable statistical properties. By ''reasonable'' we mean good
--   enough for games and test data, but not cryptography or anything where the
--   quality of the randomness really matters. 
--
--   By nature of the algorithm, the maximum value in the output is clipped
--   to (valMin + 2^31 - 1)
-- 
--   From ''Random Number Generators: Good ones are hard to find''
--   Stephen K. Park and Keith W. Miller.
--   Communications of the ACM, Oct 1988, Volume 31, Number 10.
--
randomishIntArray
	:: Shape sh
	=> sh 			-- ^ Shape of array
	-> Int 			-- ^ Minumum value in output.
	-> Int 			-- ^ Maximum value in output.
	-> Int 			-- ^ Random seed.	
	-> Array sh Int		-- ^ Array of randomish numbers.

randomishIntArray !sh !valMin !valMax !seed
	= fromVector sh $ randomishIntVector (R.size sh) valMin valMax seed


randomishIntVector 
	:: Int 			-- ^ Length of vector.
	-> Int 			-- ^ Minumum value in output.
	-> Int 			-- ^ Maximum value in output.
	-> Int 			-- ^ Random seed.	
	-> Vector Int		-- ^ Vector of randomish numbers.

randomishIntVector !len !valMin' !valMax' !seed'
 = let	-- a magic number
	-- (don't change it, the randomness depends on this specific number).
	multiplier :: Word64
	multiplier = 16807

	-- a merzenne prime
	-- (don't change it, the randomness depends on this specific number).
	modulus	:: Word64
	modulus	= 2^(31 :: Integer) - 1

	-- if the seed is 0 all the numbers in the sequence are also 0.
	seed	
	 | seed' == 0	= 1
	 | otherwise	= seed'

	!valMin	= fromIntegral valMin'
	!valMax	= fromIntegral valMax' + 1
	!range	= valMax - valMin

	{-# INLINE f #-}
	f x		= multiplier * x `mod` modulus
 in G.create 
     $ do	
	vec		<- MV.new len

	let go !ix !x 
	  	| ix == len	= return ()
		| otherwise
		= do	let x'	= f x
			MV.write vec ix $ fromIntegral $ (x `mod` range) + valMin
			go (ix + 1) x'

	go 0 (f $ f $ f $ fromIntegral seed)
	return vec


-- | Generate some randomish doubles with terrible statistical properties.
--   This just takes randomish ints then scales them, so there's not much randomness in low-order bits.
randomishDoubleArray
	:: Shape sh
	=> sh 			-- ^ Shape of array
	-> Double		-- ^ Minumum value in output.
	-> Double		-- ^ Maximum value in output.
	-> Int 			-- ^ Random seed.	
	-> Array sh Double	-- ^ Array of randomish numbers.

randomishDoubleArray !sh !valMin !valMax !seed
	= fromVector sh $ randomishDoubleVector (R.size sh) valMin valMax seed


-- | Generate some randomish doubles with terrible statistical properties.
--   This just takes randmish ints then scales them, so there's not much randomness in low-order bits.
randomishDoubleVector
	:: Int			-- ^ Length of vector
	-> Double		-- ^ Minimum value in output
	-> Double		-- ^ Maximum value in output
	-> Int			-- ^ Random seed.
	-> Vector Double	-- ^ Vector of randomish doubles.

randomishDoubleVector !len !valMin !valMax !seed
 = let	range	= valMax - valMin

	mx	= 2^(30 :: Integer) - 1
	mxf	= fromIntegral mx
	ints	= randomishIntVector len 0 mx seed
	
   in	V.map (\n -> valMin + (fromIntegral n / mxf) * range) ints
