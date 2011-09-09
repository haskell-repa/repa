
module Data.Array.Repa.Base
        ( Array
        , Repr (..)
        , Load (..))
where
import Data.Array.Repa.Shape

-- | Array type with a shape, representation tag, and element type
data family Array r sh e


-- | Operators that array representations implement differently. 
class Repr r e where
 index       :: Shape sh => Array r sh e -> sh -> e

 unsafeIndex :: Shape sh => Array r sh e -> sh -> e
 unsafeIndex = index
 
 extent      :: Shape sh => Array r sh e -> sh


-- | Load array data between representations.
--
--   * Loading between arrays of the same representation is a no-op.
--
--   * Loading a delayed array to an array-like manifest representation invokes
--     parallel computation. (unboxed vectors are array-like, but lists are not)
--
--   * Loading between manifest representations can be constant time or require a
--     parallel copy, depending on whether the two representations can be easily
--     converted.
--
class (Repr r1 e, Repr r2 e) => Load r1 r2 e where
 load :: Shape sh => Array r1 sh e  -> Array r2 sh e

