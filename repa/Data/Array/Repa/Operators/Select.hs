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
		
 where	{-# INLINE selectIO #-}
	selectIO
 	 = do	vecs		<- selectChunkedP match produce shSize 
		vecs'		<- mapM V.unsafeFreeze vecs

		-- TODO: avoid copy.
		let result	= V.concat vecs'
		
		return	(Z :. V.length result, result)
		
