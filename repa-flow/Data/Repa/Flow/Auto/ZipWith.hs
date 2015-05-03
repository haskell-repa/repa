
module Data.Repa.Flow.Auto.ZipWith
        ( zipWith3_i
        , zipWith4_i
        , zipWith5_i
        , zipWith6_i
        , zipWith7_i)
where
import Data.Repa.Flow.Auto.Base
import Data.Repa.Array.Auto
import Data.Repa.Array.Material.Auto                    (Name(..))
import qualified Data.Repa.Flow.Chunked                 as C hiding (next)
#include "repa-flow.h"


-- | Combine corresponding elements of three sources with the given function.
---
--   TODO: do this chunkwise instead. 
--         The core code has probably had an unswitch explosion.
--
zipWith3_i 
        :: ( Flow a, Flow b, Flow c
           , Build b bt, Build c ct, Build d dt)
        => (a -> b -> c -> d) 
        -> Sources a -> Sources b -> Sources c
        -> IO (Sources d)
zipWith3_i f sa sb sc
        =   C.szipWith_ii A (\_ a (b, c) -> f a b c) sa 
        =<< C.szipWith_ii A (\_ b c      -> (b, c))  sb sc
{-# INLINE zipWith3_i #-}


-- | Combine corresponding elements of four sources with the given function.
---
--   TODO: do this chunkwise instead.
--         The core code has probably had an unswitch explosion.
--
zipWith4_i 
        :: ( Flow a, Flow b, Flow c, Flow d
           , Build a at, Build b bt, Build c ct, Build d dt, Build e et)
        => (a -> b -> c -> d -> e) 
        -> Sources a -> Sources b -> Sources c -> Sources d
        -> IO (Sources e)
zipWith4_i f sa sb sc sd
 = do   sab     <- C.szipWith_ii A (\_ a b -> (a, b)) sa sb
        scd     <- C.szipWith_ii A (\_ c d -> (c, d)) sc sd
        result  <- C.szipWith_ii A (\_ (a, b) (c, d) -> f a b c d) sab scd
        return result
{-# NOINLINE zipWith4_i #-}
--  NOINLINE due to unswitch explosion.


-- | Combine corresponding elements of five sources with the given function.
---
--   TODO: do this chunkwise instead.
--         The core code has probably had an unswitch explosion.
--
zipWith5_i 
        :: ( Flow a, Flow b, Flow c, Flow d, Flow e
           , Build a at, Build b bt, Build c ct, Build d dt, Build e et
           , Build f ft)
        => (a -> b -> c -> d -> e -> f) 
        -> Sources a -> Sources b -> Sources c -> Sources d -> Sources e
        -> IO (Sources f)
zipWith5_i f sa sb sc sd se
 = do   sab     <- C.szipWith_ii  A (\_ a b -> (a, b)) sa sb
        scd     <- C.szipWith_ii  A (\_ c d -> (c, d)) sc sd
        result  <- zipWith3_i       (\(a, b) (c, d) e -> f a b c d e) 
                                    sab scd se
        return result
{-# NOINLINE zipWith5_i #-}
--  NOINLINE due to unswitch explosion.


-- | Combine corresponding elements of six sources with the given function.
---
--   TODO: do this chunkwise instead.
--         The core code has probably had an unswitch explosion.
--
zipWith6_i 
        :: ( Flow a, Flow b, Flow c, Flow d, Flow e, Flow f
           , Build a at, Build b bt, Build c ct, Build d dt, Build e et, Build f ft
           , Build g gt)
        => (a -> b -> c -> d -> e -> f -> g) 
        -> Sources a -> Sources b -> Sources c -> Sources d -> Sources e -> Sources f
        -> IO (Sources g)
zipWith6_i ff sa sb sc sd se sf
 = do   sab     <- C.szipWith_ii  A (\_ a b -> (a, b)) sa sb
        scd     <- C.szipWith_ii  A (\_ c d -> (c, d)) sc sd
        result  <- zipWith4_i       (\(a, b) (c, d) e f -> ff a b c d e f) 
                                    sab scd se sf
        return result
{-# NOINLINE zipWith6_i #-}
--  NOINLINE due to unswitch explosion.


-- | Combine corresponding elements of seven sources with the given function.
---
--   TODO: do this chunkwise instead.
--         The core code has probably had an unswitch explosion.
--
zipWith7_i 
        :: ( Flow a, Flow b, Flow c, Flow d, Flow e, Flow f, Flow g
           , Build a at, Build b bt, Build c ct, Build d dt, Build e et, Build f et, Build g gt
           , Build h ht)
        => (a -> b -> c -> d -> e -> f -> g -> h) 
        -> Sources a -> Sources b -> Sources c -> Sources d -> Sources e -> Sources f -> Sources g
        -> IO (Sources h)

zipWith7_i ff sa sb sc sd se sf sg
 = do   sab     <- C.szipWith_ii  A (\_ a b -> (a, b)) sa sb
        scd     <- C.szipWith_ii  A (\_ c d -> (c, d)) sc sd
        result  <- zipWith5_i       (\(a, b) (c, d) e f g -> ff a b c d e f g) 
                                    sab scd se sf sg
        return result
{-# NOINLINE zipWith7_i #-}
--  NOINLINE due to unswitch explosion.



