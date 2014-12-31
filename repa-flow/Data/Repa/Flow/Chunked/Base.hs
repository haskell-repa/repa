
module Data.Repa.Flow.Chunked.Base
        ( Sources, Sinks
        , Flow)
where
import Data.Repa.Array                    as A
import qualified Data.Repa.Flow.Generic   as G


-- | A bundle of sources, where the elements are chunked into arrays.
type Sources i m r e
        = G.Sources i m (A.Vector r e)


-- | A bundle of sinks,   where the elements are chunked into arrays.
type Sinks   i m r e
        = G.Sinks   i m (A.Vector r e)


-- | Shorthand for common type classes.
type Flow i m r a
        = (Ord i, Monad m, Bulk r DIM1 a)

