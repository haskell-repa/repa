{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}

module StrictComplex
	( Complex(..)
	, mag)
where
import 	Data.Array.Parallel.Base ( (:*:)(..) )
type 	Complex = Double :*: Double

instance Num Complex where
  (r :*: i) + (r' :*: i') = (r+r' :*: i+i')
  (r :*: i) - (r' :*: i') = (r-r' :*: i-i')
  (r :*: i) * (r' :*: i') = (r*r' - i*i' :*: r*i' + r'*i)
  fromInteger n = (fromInteger n :*: 0.0)



mag :: Complex -> Double
mag (r :*: i)	= sqrt (r * r + i * i)


