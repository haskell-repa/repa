
module Data.Array.Repa.Array
	(Array	(..))
where
import qualified Data.Array.Parallel.Unlifted	as U
	
data Array sh e 
	= Manifest sh (U.Array e)
	| Delayed  sh (sh -> e)
