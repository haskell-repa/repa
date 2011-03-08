{-# LANGUAGE BangPatterns #-}

module Data.Array.Repa.Internals.Select
	(selectChunkedS)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Gang
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import GHC.Base					(remInt, quotInt)
import Prelude					as P


-- | Select indices matching a predicate
selectChunkedS 
	:: (Shape sh, Unbox a)
	=> (sh -> Bool)		-- ^ See if this predicate matches.
	-> (sh -> a)		-- ^  .. and apply fn to the matching index
	-> IOVector a		-- ^  .. then write the result into the vector.
	-> sh 			-- ^ Extent of indices to apply to predicate.
	-> IO Int		-- ^ Number of elements written to destination array.
{-# INLINE selectChunkedS #-}
selectChunkedS match produce !vDst !shSize
 = fill 0 0
 where	lenSrc	= size shSize
	lenDst	= VM.length vDst
	
	fill !nSrc !nDst
	 | nSrc >= lenSrc	= return nDst
	 | nDst >= lenDst	= return nDst
	
	 | ixSrc	<- fromIndex shSize nSrc
	 , match ixSrc 
	 = do	VM.unsafeWrite vDst nDst (produce ixSrc)
		fill (nSrc + 1) (nDst + 1)

	 | otherwise
	 = 	fill (nSrc + 1) nDst
