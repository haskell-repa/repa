{-# OPTIONS -fno-warn-unused-imports #-}
-- | Repa arrays are wrappers around a linear structure that holds the element
--   data. 
-- 
--  The representation tag determines what structure holds the data.
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
--   * `S`  -- Hints that computing this array is a small amount of work,
--             so computation should be sequential rather than parallel to avoid
--             scheduling overheads.
-- 
--   * `I`  -- Hints that computing this array will be an unbalanced workload,
--             so computation of successive elements should be interleaved between
--             the processors
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
--  5. Scheduling an 8-thread parallel computation can take 50us on a Linux machine. 
--     You should switch to sequential evaluation functions like `computeS` and
--     `foldS` for small arrays in inner loops, and at the bottom of a 
--     divide-and-conquer algorithm. Consider using a `computeP` that evaluates
--     an array defined using `computeS` or `foldS` for each element.
--
--  6. Compile the modules that use Repa with the following flags:
--     @-Odph -rtsopts -threaded@
--     @-fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000@
--     @-fllvm -optlo-O3@
--     You don't want the liberate-case transform because it tends to duplicate
--     too much intermediate code, and is not needed if you use bang patterns
--     as per point 4. The unfolding flags tell the inliner to not to fool around with 
--     heuristics, and just inline everything. If the binaries become too big then 
--     split the array part of your program into separate modules and only compile
--     those with the unfolding flags.
--
--  7. Repa writes to the GHC eventlog at the start and end of  each parallel computation.
--     Use threadscope to see what your program is doing.
--
--  8. When you're sure your program works, switch to the unsafe versions
--     of functions like `traverse`. These don't do bounds checks.
--
-- /Changes for Repa 3.2:/
--
--  1. Renamed some Repa 3.1 type classes to have more intuitive names: 
--     `Repr` -> `Source`, `Fill` -> `Load`, `Fillable` -> `Target`, `Combine` -> `Structured`.
--
--  2. Also renamed `MArray` -> `MVec` to emphasise its linear structure.
--
--  3. Made `Array` and `MVec` associated types of `Source` and `Target` respectively.
--
--  4. Added the `S` (Smallness) and `I` (Interleave) hints.
--
module Data.Array.Repa
        ( -- * Abstract array representation
          module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , Array (..)
        , Source(..), (!), toList
        , deepSeqArrays

        -- * Computation
        , computeP, computeS
        , copyP,    copyS

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
        , Structured(..)

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
        -- ** Selection
        , selectP)
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
import Data.Array.Repa.Repr.HintSmall
import Data.Array.Repa.Repr.HintInterleave
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Undefined           ()
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Operators.IndexSpace
import Data.Array.Repa.Operators.Interleave
import Data.Array.Repa.Operators.Reduction
import Data.Array.Repa.Operators.Selection
import Data.Array.Repa.Arbitrary                ()
import Prelude          ()



