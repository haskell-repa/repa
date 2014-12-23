module Data.Repa.Flow
        ( Flows         (..)
        , Flow
        , Source, Sink

          -- * Mapping
        , map_i,        map_o
        , map_ci,       map_co

          -- * IO
        , fileSourceRecords)
where
import Data.Repa.Array                    as A
import Data.Repa.Array.Foreign            as A
import Data.Repa.Eval.Array               as A
import qualified Data.Repa.Flow.Simple    as S
import qualified Data.Repa.Flow.Gang      as G
import Data.Word

data Sources i m r e
        = Sources !(G.Sources i m (A.Vector r e))

data Sinks   i m r e
        = Sinks   !(G.Sinks   i m (A.Vector r e))


-- Map ------------------------------------------------------------------------
-- | Map a function over elements pulled from a source.
map_i   :: (Flow m r1 a, Flow m r2 b, A.Target r2 b)
        => (a -> b) -> Source m r1 a -> m (Source m r2 b)
map_i f (VSource s0)
 = do   s1      <- S.map_i (A.computeS . A.map f) s0
        return  $ VSource s1
{-# INLINE [2] map_i #-}


-- | Map a function over elements pushed into a sink.
map_o   :: (Flow m r1 b, Flow m r2 a, A.Target r1 b)
        => (a -> b) -> Sink m r1 b -> m (Sink m r2 a)
map_o f (VSink s0)
 = do   s1      <- S.map_o (A.computeS . A.map f) s0
        return  $ VSink s1
{-# INLINE [2] map_o #-}


-- | Map a function over elements pulled from a source, a chunk at a time.
--
--   TODO: expose index.   
mapc_i  :: (Flow m r1 a, Flow m r2 b)
        => (Int -> Vector r1 a -> Vector r2 b)
        -> Sources m r1 a -> m (Sources m r2 b)
mapc_i f (VSource s0)
 = do   s1      <- G.maps_i f s0
        return  $  VSource s1
{-# INLINE [2] map_ci #-}


-- | Map a function over elements pushed to a sink, a chunk at a time.
--
--   TODO: expose index.
map_co  :: (Flow m r1 b, Flow m r2 a)
        => (Vector r1 a -> Vector r2 b) -> Sink m r2 b -> m (Sink m r1 a)
map_co f (VSink s0)
 = do   s1      <- S.map_o f s0
        return  $ VSink s1
{-# INLINE [2] map_co #-}


-------------------------------------------------------------------------------
{-
distributes_o
        :: (Flow r1 m a)
        -> Sinks a
        -> ((Int, a) -> IO ())
        -> IO (Sink r m (Vector r2 a))
-}

-- IO -------------------------------------------------------------------------
fileSourceRecords
        :: FilePath
        -> Int
        -> (Word8 -> Bool)
        -> IO ()
        -> IO (Source IO F Word8)

fileSourceRecords filePath len pSep aFail
 = do   s1      <- S.fileSourceRecords filePath len pSep aFail
        return  $ VSource s1



