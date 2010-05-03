{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}

-- | Strict complex doubles.
module Data.Array.Repa.Algorithms.Complex
	( Complex
	, mag
	, arg
	, (:*:)(..))
where
import 	Data.Array.Parallel.Base ((:*:)(..))

-- | Strict complex doubles.
type Complex 
	= Double :*: Double

instance Num Complex where
  abs x				= (mag x) :*: 0
  signum (re :*: _)		= signum re :*: 0
  fromInteger n			= fromInteger n :*: 0.0
  (r :*: i) + (r' :*: i')	= r+r' :*: i+i'
  (r :*: i) - (r' :*: i')	= r-r' :*: i-i'
  (r :*: i) * (r' :*: i')	= r*r' - i*i' :*: r*i' + r'*i


instance Fractional Complex where
  (a :*: b) / (c :*: d)		
 	= let	den	= c^(2 :: Int) + d^(2 :: Int)
		re	= (a * c + b * d) / den
		im	= (b * c - a * d) / den
	  in	re :*: im
	
  fromRational x	= fromRational x :*: 0
	
-- | Take the magnitude of a complex number.
mag :: Complex -> Double
mag (r :*: i)	= sqrt (r * r + i * i)


-- | Take the argument (phase) of a complex number, in the range [-pi .. pi].
arg :: Complex -> Double
arg (re :*: im)
 = normaliseAngle $ atan2 im re

 where 	normaliseAngle :: Double -> Double
	normaliseAngle f
	 | f < - pi	
	 = normaliseAngle (f + 2 * pi)
	
	 | f > pi
	 = normaliseAngle (f - 2 * pi)

	 | otherwise
	 = f
