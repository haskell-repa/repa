{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Operators.Selection
	(select)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Selection
import Data.Array.Repa.Repr.Unboxed             as U
import qualified Data.Vector.Unboxed		as V
import System.IO.Unsafe


-- | Produce an array by applying a predicate to a range of integers.
--   If the predicate matches, then use the second function to generate
--   the element.
--
--   * This is a low-level function helpful for writing filtering
--     operations on arrays.
--
--   * Use the integer as the index into the array you're filtering.
--
select	:: Unbox a
        => (Int -> Bool)	-- ^ If the Int matches this predicate,
	-> (Int -> a)		-- ^  ... then pass it to this fn to produce a value
	-> Int			-- ^ Range between 0 and this maximum.
	-> Array U DIM1 a	-- ^ Array containing produced values.

{-# INLINE [2] select #-}
select match produce len
 = unsafePerformIO
 $ do   (sh, vec)	<- selectIO
	return $ sh `seq` vec `seq`
	         fromUnboxed sh vec

 where	{-# INLINE selectIO #-}
	selectIO
 	 = do	vecs		<- selectChunkedP match produce len
		vecs'		<- mapM V.unsafeFreeze vecs

		-- TODO: avoid copy somehow.
		let result	= V.concat vecs'

		return	(Z :. V.length result, result)
