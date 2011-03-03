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

{-# INLINE [2] force #-}	
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
{-# INLINE [2] force2 #-}	
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
		-- NOTE We must specialise this for the common case of two regions to enable
		--      fusion for them. If we just have the next case (arbitrary region list)
		--      the worker won't fuse with the filling / evaluation code.
		Array sh [r1]
		 -> do	mvec	<- VM.new (S.size sh)
			fillRegion2P mvec sh r1
			vec	<- V.unsafeFreeze mvec
			return (sh, vec)

		Array sh [r1, r2]
	 	 -> do	mvec	<- VM.new (S.size sh)
			fillRegion2P mvec sh r1
			fillRegion2P mvec sh r2
			vec	<- V.unsafeFreeze mvec
			return (sh, vec)
	
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

{-# INLINE [1] fillRegionP #-}
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
	=> VM.IOVector a	-- ^ Vector to write elements into.
	-> DIM2			-- ^ Extent of entire array.
	-> Region DIM2 a	-- ^ Region to fill.
	-> IO ()
	
{-# INLINE [1] fillRegion2P #-}
fillRegion2P mvec sh@(_ :. height :. width) (Region range gen)
 = mvec `seq` height `seq` width `seq`
   case range of 
	RangeAll	
	 -> fillRect2 mvec sh gen 
		(Rect 	(Z :. 0          :. 0) 
			(Z :. height - 1 :. width - 1))

	RangeRects _ [rect]
	 -> fillRect2 mvec sh gen rect 

	-- Specialise for the common case of 4 rectangles so we get fusion.
	-- The following case with mapM_ doesn't fuse because mapM_ isn't completely unrolled.
	RangeRects _ [r1, r2, r3, r4]
	 -> do	fillRect2 mvec sh gen r1
		fillRect2 mvec sh gen r2
		fillRect2 mvec sh gen r3
		fillRect2 mvec sh gen r4

	RangeRects _ rects
	 -> mapM_ (fillRect2 mvec sh gen) rects

		
-- | Fill a rectangle in a vector.
fillRect2 
	:: Elt a
	=> VM.IOVector a	-- ^ Vector to write elements into.
	-> DIM2 		-- ^ Extent of entire array.
	-> Generator DIM2 a	-- ^ Generator for array elements.
	-> Rect DIM2		-- ^ Rectangle to fill.
	-> IO ()

{-# INLINE fillRect2 #-}	
fillRect2 mvec sh@(_ :. _ :. width) gen (Rect (Z :. y0 :. x0) (Z :. y1 :. x1)) 
 = mvec `seq` width `seq` y0 `seq` x0 `seq` y1 `seq` x1 `seq` 
   case gen of
	GenManifest{}
	 -> error "fillRegion2P: GenManifest, copy elements."
	
	-- If the region we're filling is just one pixel wide then just fill it
	--   in the current thread instead of starting up the whole gang.
	GenDelayed getElem
	 |  x0 == x1
	 -> fillVectorBlock mvec
		(getElem . fromIndex sh)
		width x0 y0 x1 y1

	 |  y0 == y1
	 -> fillVectorBlock mvec
		(getElem . fromIndex sh)
		width x0 y0 x1 y1
	
	 | otherwise
	 -> fillVectorBlockP mvec
		(getElem . fromIndex sh) 
		width x0 y0 x1 y1
	
	-- Cursor based arrays.
	GenCursor makeCursor shiftCursor loadElem
         -> fillCursoredBlock2P mvec
		makeCursor shiftCursor loadElem
		width x0 y0 x1 y1
