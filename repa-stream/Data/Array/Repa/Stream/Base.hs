
module Data.Array.Repa.Stream.Base
        ( -- * Linear Streams
          Stream (..)
        , Step   (..)
        , Size   (..)
        , stream
        , streamOfChain
        , fold
        , foldM

          -- * Distributed Streams
        , DistStream (..)
        , streamD
        , foldD
        , foldMD)
where
import GHC.Exts
import qualified Data.Array.Repa.Chain.Base     as C


-- Linear ---------------------------------------------------------------------
-- | Pure streams.
--
--   At creation time, streams can have a known or unknown length. 
-- 
--   If the length is unknown at creation time, you must evaluate all elements
--   to determine the final length.
-- 
data Stream a
        = forall s
        . Stream 
        { streamSize       :: Size
        , streamStateStart :: s
        , streamMkStep     :: s -> Step s a
        }


-- | A stream command.
data Step s a
        = Yield  s a            -- ^ Yield a new stream state and an element.
        | Update s              -- ^ Just update the stream state.
        | Done                  -- ^ Signal that the stream is finished.
        deriving (Show)


-- | The known size of a stream.
data Size
        = Exact   Int#          -- ^ Stream produces exactly this many elements.
        | Max     Int#          -- ^ Stream produces at most this many elements.
        | Unknown               -- ^ Stream produces some number of elemements 
                                --   which was unknown at creation time.
        deriving Show


-- | Construct a stream.
stream  :: Int#                 -- ^ Overall size of the stream.
        -> (Int# -> a)          -- ^ Get the element at this position.
        -> Stream a

stream size get
 = Stream (Exact size) 0 mkStep
 where  mkStep (I# ix)
         | ix >=# size  = Done
         | otherwise    = Yield (I# (ix +# 1#)) (get ix)
        {-# INLINE [0] mkStep #-}
{-# INLINE [1] stream #-}


-- | Convert a chain to a stream.
---
--   Notice that when we do this we need to introduce a loop counter.
streamOfChain :: C.Chain a -> Stream a
streamOfChain (C.Chain len s0 mkStep)
 = Stream (Exact len) (0, s0) mkStep'
 where  
        mkStep' (I# ix, s)
         | ix >=# len  = Done
         | otherwise
         = case mkStep ix s of
                C.Yield s' x    -> Yield  (I# (ix +# 1#), s') x
                C.Update s'     -> Update (I# ix,         s')
        {-# INLINE [0] mkStep' #-}
{-# INLINE [1] streamOfChain #-}


-- | Consume a stream.
fold :: (a -> b -> b) -> b -> Stream a -> b
fold f y0 (Stream _size s0 mkStep)
 = eat s0 y0
 where  eat s y
         = case mkStep s of
                Yield  s' x     -> eat s' (f x y)
                Update s'       -> eat s' y
                Done            -> y
        {-# INLINE [0] eat #-}


-- | Consume a stream in a monad.
foldM  :: Monad m 
        => (a -> b -> m b) -> b -> Stream a -> m b

foldM f y0 (Stream _size s0 mkStep)
 = eat s0 y0
 where  eat s y
         = case mkStep s of
                Yield s' x      
                 -> do  y'      <- f x y
                        eat s' y'
                Update s'       -> eat s' y
                Done            -> return y
        {-# INLINE [0] eat #-}


-- Fragmented -----------------------------------------------------------------
-- | Pure fragmented streams.
--
--   The stream is broken into several fragments that can be evaluated
--   concurrently.
--
data DistStream a
        = DistStream
                Size                    -- Overall size of stream.
                Int#                    -- Number of fragments.
                (Int# -> Stream a)      -- Get a stream fragment.


-- | Construct a fragmented stream.
streamD :: Size                         -- ^ Overall size of stream.
        -> Int#                         -- ^ Number of fragments.
        -> (Int# -> Int#)               -- ^ Get the start position of a fragment.
        -> (Int# -> a)                  -- ^ Get the element at this position.
        -> DistStream a

streamD size frags start get
 = DistStream size frags getFrag
 where  getFrag frag 
         = stream (start (frag +# 1#) -# start frag)
                  get
        {-# INLINE [0] getFrag #-}
{-# INLINE streamD #-}


-- | Consume a fragmented stream.
foldD :: (a -> b -> b) -> b -> DistStream a -> b
foldD f y0 (DistStream _size frags getFrag)
 = eatFrags y0 0#
 where  eatFrags y frag
         | frag >=# frags       = y
         | otherwise            
         = eatFrags (fold f y (getFrag frag))
                    (frag +# 1#)
        {-# INLINE [0] eatFrags #-}
{-# INLINE [1] foldD #-}


-- | Consume a fragmented stream, in a monad.
foldMD  :: Monad m
        => (a -> b -> m b) -> b -> DistStream a -> m b

foldMD f y0 (DistStream _size frags getFrag)
 = eatFrags y0 0#
 where  eatFrags y frag
         | frag >=# frags       = return y
         | otherwise
         = do   y'      <- foldM f y (getFrag frag)
                eatFrags y' (frag +# 1#)
        {-# INLINE [0] eatFrags #-}
{-# INLINE [1] foldMD #-}
