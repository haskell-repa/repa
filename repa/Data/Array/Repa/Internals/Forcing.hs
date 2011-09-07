{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.Forcing
	( toVector
	, toList
	, force,  forceWith
	, force2, forceWith2)
where
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
-- | Convert an array to an unboxed `Data.Vector`, forcing it if required.
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

		Array sh _
		 -> do	mvec	<- VM.unsafeNew (S.size sh)
                        forceWith (VM.unsafeWrite mvec) arr'
			vec	<- V.unsafeFreeze mvec
			return	(sh, vec)


-- | Force an array, passing elements to the provided update function.
--   Provide something like @(Foreign.Ptr.pokeElemOff ptr)@ to write elements into a buffer.
--   The array is split into linear chunks and each chunk is evaluated in parallel.
forceWith
        :: (Shape sh, Elt a)
        => (Int -> a -> IO ())
        -> Array sh a
        -> IO ()

{-# INLINE [2] forceWith #-}        
forceWith !update arr@(Array sh _)
        = fillChunkedP  
                (S.size sh)
		update
		(\ix -> arr `unsafeIndex` fromIndex sh ix)


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
 	 = arr' `deepSeqArray`
	   case arr' of
		-- Don't force an already forced array.
		Array sh [Region RangeAll (GenManifest vec)]
	 	 -> 	return (sh, vec)

		-- Create a vector to hold the new array and load in the regions.
		Array sh _
		 -> do	mvec	<- VM.new (S.size sh)
                        forceWith2 (VM.unsafeWrite mvec) arr'
                        vec     <- V.unsafeFreeze mvec
                        return (sh, vec)


-- | Force an array, passing elements to the provided update function.
--   Provide something like @(Foreign.Ptr.pokeElemOff ptr)@ to write elements into a buffer.
--   This forcing function is specialised for DIM2 arrays, and does blockwise filling.
forceWith2
        :: Elt a
        => (Int -> a -> IO ())
        -> Array DIM2 a
        -> IO ()

{-# INLINE [2] forceWith2 #-}
forceWith2 !write arr
 = arr `deepSeqArray`
   case arr of
	-- If the array is already manifest then copy it into the buffer.
	-- We don't need a particular traversal order just for a copy.
	Array _ [Region RangeAll (GenManifest _)]
 	 -> forceWith write arr

	-- NOTE We must specialise this for common numbers of regions so that
	--      we get fusion for them. If we just have the last case (arbitrary
	--      region list) then the worker won't fuse with the filling /
	--      evaluation code.
	Array sh [r1]
	 -> do	fillRegion2P write sh r1

	Array sh [r1, r2]
 	 -> do	fillRegion2P write sh r1
		fillRegion2P write sh r2

	Array sh regions
 	 -> do	mapM_ (fillRegion2P write sh) regions


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
	=> (Int -> a -> IO ())	-- ^ Update function to write into result buffer
	-> DIM2			-- ^ Extent of entire array.
	-> Region DIM2 a	-- ^ Region to fill.
	-> IO ()

{-# INLINE [1] fillRegion2P #-}
fillRegion2P write sh@(_ :. height :. width) (Region range gen)
 = write `seq` height `seq` width `seq`
   case range of
	RangeAll
	 -> fillRect2 write sh gen
		(Rect 	(Z :. 0          :. 0)
			(Z :. height - 1 :. width - 1))

	RangeRects _ [r1]
	 -> do  fillRect2 write sh gen r1

	RangeRects _ [r1, r2]
	 -> do	fillRect2 write sh gen r1
		fillRect2 write sh gen r2

	RangeRects _ [r1, r2, r3]
	 -> do	fillRect2 write sh gen r1
		fillRect2 write sh gen r2
		fillRect2 write sh gen r3

	RangeRects _ [r1, r2, r3, r4]
	 -> do	fillRect2 write sh gen r1
		fillRect2 write sh gen r2
		fillRect2 write sh gen r3
		fillRect2 write sh gen r4

	RangeRects _ rects
	 -> mapM_ (fillRect2 write sh gen) rects


-- | Fill a rectangle in a vector.
fillRect2
	:: Elt a
	=> (Int -> a -> IO ())	-- ^ Update function to write into result buffer
	-> DIM2 		-- ^ Extent of entire array.
	-> Generator DIM2 a	-- ^ Generator for array elements.
	-> Rect DIM2		-- ^ Rectangle to fill.
	-> IO ()

{-# INLINE fillRect2 #-}
fillRect2 write sh@(_ :. _ :. width) gen (Rect (Z :. y0 :. x0) (Z :. y1 :. x1))
 = write `seq` width `seq` y0 `seq` x0 `seq` y1 `seq` x1 `seq`
   case gen of
	GenManifest vec
	 -> fillCursoredBlock2P write
		id addDim (\ix -> vec `V.unsafeIndex` toIndex sh ix)
		width x0 y0 x1 y1

	-- Cursor based arrays.
	GenCursor makeCursor shiftCursor loadElem
         -> fillCursoredBlock2P write
		makeCursor shiftCursor loadElem
		width x0 y0 x1 y1
