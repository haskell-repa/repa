{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

module Data.Array.Repa.Operators.Select
	(select)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base
import Data.Array.Repa.Internals.Select
import Data.Array.Repa.Shape			as S
import qualified Data.Vector.Unboxed		as V
import qualified Data.Vector.Unboxed.Mutable	as VM
import System.IO.Unsafe


select	:: (Shape sh, Elt a)
	=> (sh -> Bool)
	-> (sh -> a)
	-> sh
	-> Array DIM1 a
	
{-# INLINE select #-}
select match produce shSize
 = unsafePerformIO 
 $ do	(sh, vec)	<- selectIO 
	return $ sh `seq` vec `seq` 
		 Array sh [Region RangeAll (GenManifest vec)]
		
 where	selectIO
 	 = do	mvec	<- VM.new (S.size shSize)
		len	<- selectChunkedS match produce mvec shSize 
		vec	<- V.unsafeFreeze mvec
		return	(Z :. len, V.slice 0 len vec)
		
