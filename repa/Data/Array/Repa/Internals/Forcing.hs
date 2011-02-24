{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.Forcing
	( toVector
	, toList
	, force, force2)
where
import Data.Array.Repa.Internals.EvalBlockwise
import Data.Array.Repa.Internals.EvalChunked
import Data.Array.Repa.Internals.EvalCursored
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Shape			as S
import qualified Data.Vector.Unboxed		as V
import qualified Data.Vector.Unboxed.Mutable	as VM
import Data.Vector.Unboxed			(Vector)
import System.IO.Unsafe

stage	= "Data.Array.Repa.Internals.Forcing"


-- Conversions that also force the array ----------------------------------------------------------
-- | Convert an array to an unboxed `U.Array`, forcing it if required.
--	The elements come out in row-major order.
toVector
	:: (Shape sh, Elt a)
	=> Array sh a 
	-> Vector a
{-# INLINE toVector #-}
toVector arr
 = case force arr of
	Array _ [Region _ (GenManifest vec)]	-> vec
	_	-> error $ stage ++ ".toVector: force failed"


-- | Convert an array to a list, forcing it if required.
toList 	:: (Shape sh, Elt a)
	=> Array sh a
	-> [a]

{-# INLINE toList #-}
toList arr
 = V.toList $ toVector arr


-- Forcing ----------------------------------------------------------------------------------------
-- | Force an array, so that it becomes `Manifest`.
--   The array is split into linear chunks and each chunk evaluated in parallel.
force	:: (Shape sh, Elt a)
	=> Array sh a -> Array sh a

{-# NOINLINE force #-}	
force arr
 = unsafePerformIO
 $ do	(sh, vec)	<- forceIO arr
	return $ sh `seq` vec `seq` 
		 Array sh [Region RangeAll (GenManifest vec)]
	
 where	forceIO arr'
	 = case arr' of
		-- Don't force an already forced array.
		Array sh [Region RangeAll (GenManifest vec)]
		 -> 	return (sh, vec)

		-- Create the vector to hold the new array and load in the regions.
		Array sh regions
		 -> do	mvec	<- VM.unsafeNew (S.size sh)
			mapM_ (fillRegionP mvec sh) regions
			vec	<- V.unsafeFreeze mvec
			return (sh, vec)


-- | Force an array, so that it becomes `Manifest`.
--   This forcing function is specialised for DIM2 arrays, and does blockwise filling.
force2	:: Elt a => Array DIM2 a -> Array DIM2 a
{-# NOINLINE force2 #-}	
force2 arr
 = unsafePerformIO 
 $ do	(sh, vec)	<- forceIO2 arr
	return $ sh `seq` vec `seq` 
		 Array sh [Region RangeAll (GenManifest vec)]

 where	forceIO2 arr'
 	 = case arr' of
		-- Don't force an already forced array.
		Array sh [Region RangeAll (GenManifest vec)]
	 	 -> 	return (sh, vec)
	
		-- Create a vector to hold the new array and load in the regions.
		Array sh regions
	 	 -> do	mvec	<- VM.new (S.size sh)
			mapM_ (fillRegion2P mvec sh) regions
			vec	<- V.unsafeFreeze mvec
			return (sh, vec)
			

-- FillRegionP ------------------------------------------------------------------------------------
-- | Fill an array region into this vector.
fillRegionP 
	:: (Shape sh, Elt a)
	=> VM.IOVector a
	-> sh
	-> Region sh a
	-> IO ()

{-# INLINE fillRegionP #-}
fillRegionP mvec sh (Region RangeAll gen)
 = case gen of
	GenManifest{}
	 -> error "fillRegionP: GenManifest, copy elements."
	
	GenDelayed getElem
	 -> fillChunkedS mvec (getElem . fromIndex sh)
	
	GenCursor makeCursor _ loadElem
	 -> fillChunkedS mvec (loadElem . makeCursor . fromIndex sh)

fillRegionP _ _ _
	= error "fillRegionP: not finished for ranges"


-- FillRegion2P -----------------------------------------------------------------------------------	
-- | Fill an array region into a vector.
--   This is specialised for DIM2 regions.
--   The region is evaluated in parallel in a blockwise manner, where each block is
--   evaluated independently and in a separate thread. For delayed or cursored regions
--   access their source elements from the local neighbourhood, this specialised version
--   should given better cache performance than plain `fillRegionP`.
--
fillRegion2P 
	:: Elt a
	=> VM.IOVector a
	-> DIM2			-- ^ Extent of entire array.
	-> Region DIM2 a
	-> IO ()
	
{-# INLINE fillRegion2P #-}
fillRegion2P mvec sh@(_ :. height :. width) (Region RangeAll gen)
 = mvec `seq` height `seq` width `seq`
   case gen of
	GenManifest{}
	 -> error "fillRegion2P: GenManifest, copy elements."
	
	GenDelayed getElem
	 -> fillVectorBlockwiseP mvec (getElem . fromIndex sh) width
	
	-- TODO: we're faking the range by only filling the inner elements.
	GenCursor makeCursor shiftCursor loadElem
	 -> do	
		-- fill the inner partition
		let x0	= 1
		let x1	= width - 2
		let y0	= 1
		let y1	= height - 2

		-- FUCKING IMPORTANT: if we're not going to initialize the whole array
		-- then we must zero it, otherwise the result will be undefined when normalised.
		VM.set mvec zero

		fillCursoredBlock2P mvec
			makeCursor shiftCursor loadElem
			width x0 y0 x1 y1

{-		fillVectorBlock mvec
			(loadElem . makeCursor . fromIndex sh) 
			width x0 y0 x1 y1
-}

fillRegion2P _ _ _
	= error "fillRegion2P: not finished for ranges"

