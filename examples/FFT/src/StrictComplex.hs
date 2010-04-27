{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}

module StrictComplex
	( Complex(..)
	, mag
	, (:*:)(..))
where
import 	Data.Array.Parallel.Base ((:*:)(..))

-- | Strict complex doubles.
type Complex 
	= Double :*: Double

instance Num Complex where
  (r :*: i) + (r' :*: i')	= r+r' :*: i+i'
  (r :*: i) - (r' :*: i')	= r-r' :*: i-i'
  (r :*: i) * (r' :*: i')	= r*r' - i*i' :*: r*i' + r'*i
  fromInteger n			= fromInteger n :*: 0.0

instance Fractional Complex where
  (a :*: b) / (c :*: d)		
 	= let	den	= c^2 + d^2
		re	= (a * c + b * d) / den
		im	= (b * c - a * d) / den
	  in	re :*: im
	
	
-- | Take the magnitude of a complex number.
mag :: Complex -> Double
mag (r :*: i)	= sqrt (r * r + i * i)


