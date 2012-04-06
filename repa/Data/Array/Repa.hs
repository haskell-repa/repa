
-- | Repa arrays are wrappers around a linear structure that holds the element
--   data. The representation tag determines what structure holds the data.
--
--   Delayed Representations (functions that compute elements)
--
--   * `D`  -- Functions from indices to elements.
--
--   * `C`  -- Cursor functions.
--
--   Manifest Representations (real data)
--
--   * `U`  -- Adaptive unboxed vectors.
--
--   * `V`  -- Boxed vectors.
--
--   * `B`  -- Strict ByteStrings.
--
--   * `F`  -- Foreign memory buffers.
--
--   Meta Representations
--
--   * `P`  -- Arrays that are partitioned into several representations.
--
--   * `X`  -- Arrays whose elements are all undefined.
--
--  Array fusion is achieved via the delayed (`D`) and cursored (`C`)
--  representations. At compile time, the GHC simplifier combines the functions
--  contained within `D` and `C` arrays without needing to create manifest
--  intermediate arrays. 
--
--
--  /Advice for writing fast code:/
--
--  1. Repa does not support nested parallellism. 
--     This means that you cannot `map` a parallel worker function across
--     an array and then call `computeP` to evaluate it, or pass a parallel
--     worker to parallel reductions such as `foldP`. If you do then you will
--     get a run-time warning and the code will run very slowly.
--
--  2. Arrays of type @(Array D sh a)@ or @(Array C sh a)@ are /not real arrays/.
--     They are represented as functions that compute each element on demand.
--     You need to use `computeS`, `computeP`, `computeUnboxedP`
--     and so on to actually evaluate the elements.
--     
--  3. Add @INLINE@ pragmas to all leaf-functions in your code, expecially ones
--     that compute numeric results. Non-inlined lazy function calls can cost
--     upwards of 50 cycles each, while each numeric operator only costs one (or less).
--     Inlining leaf functions also ensures they are specialised at the appropriate
--     numeric types.
--     
--  4. Add bang patterns to all function arguments, and all fields of your data
--     types. In a high-performance Haskell program, the cost of lazy evaluation
--     can easily dominate the run time if not handled correctly. You don't want
--     to rely on the strictness analyser in numeric code because if it does not
--     return a perfect result then the performance of your program will be awful.
--     This is less of a problem for general Haskell code, and in a different
--     context relying on strictness analysis is fine.
--
--  5. Scheduling a parallel computation takes about 200us on an OSX machine. 
--     You should switch to sequential evaluation functions like `computeS` and
--     `foldS` for small arrays in inner loops, and at the bottom of a 
--     divide-and-conquer algorithm. Consider using a `computeP` that evaluates
--     an array defined using `computeS` or `foldS` for each element.
--
--  6. Compile your program with 
--     @-Odph -rtsopts -threaded -fno-liberate-case -fllvm -optlo-O3@
--     You don't want the liberate-case transform because it tends to duplicate
--     too much intermediate code, and is not needed if you use bang patterns
--     as per point 4. 
--
--  7. The implementation writes to the GHC eventlog at the start and end of 
--     each parallel computation. Use threadscope to see what your program is doing.
--
module Data.Array.Repa
        ( -- * Abstract array representation
          module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , Array(..)
        , Repr(..), (!), toList
        , deepSeqArrays

        -- * Converting between array representations
        , computeP, computeS
        , copyP,    copyS
        , now

        -- * Concrete array representations
        -- ** Delayed representation
        , D, fromFunction, toFunction
        , delay

        -- ** Unboxed vector representation
        , U
        , computeUnboxedP, computeUnboxedS
        , fromListUnboxed
        , fromUnboxed
        , toUnboxed
                
	-- from Data.Array.Repa.Operators.IndexSpace ----------------
        -- * Operators
	-- ** Index space transformations
	, reshape
	, append, (++)
        , extract
	, transpose
	, backpermute
	, backpermuteDft

        -- ** Slice transformations
	, module Data.Array.Repa.Slice
	, slice
        , extend

	-- from Data.Array.Repa.Operators.Mapping -------------------
        -- ** Structure preserving operations
	, map
	, zipWith
	, (+^), (-^), (*^), (/^)
        , Combine(..)

	-- from Data.Array.Repa.Operators.Traversal ------------------
	-- ** Generic traversal
	, traverse 
	, traverse2
	, traverse3
	, traverse4
	
	-- from Data.Array.Repa.Operators.Interleave -----------------
	-- ** Interleaving
	, interleave2
	, interleave3
	, interleave4
	
	-- from Data.Array.Repa.Operators.Reduction ------------------
	-- ** Reduction
	, foldP,    foldS
	, foldAllP, foldAllS
	, sumP,     sumS
	, sumAllP,  sumAllS
        , equalsP,  equalsS
	
	-- from Data.Array.Repa.Operators.Selection ------------------
	, select)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Undefined           ()
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Operators.IndexSpace
import Data.Array.Repa.Operators.Interleave
import Data.Array.Repa.Operators.Reduction
import Data.Array.Repa.Operators.Selection
import Prelude          ()



