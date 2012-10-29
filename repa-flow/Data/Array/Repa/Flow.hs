{-# LANGUAGE MagicHash, BangPatterns, ExistentialQuantification #-}
-- Flows are stateful and incremental.
-- Taking a prefix only computes those elements.
-- 
-- Use dup2 for sharing, caches only as much data as required to 
-- handle the desync in the program.
--
module Data.Array.Repa.Flow
        ( Flow (..) 

        -- * Conversion
        , flow
        , unflow

        -- * Construction
        , generate
        , replicate
        , enumFromN

        -- * Pure combinators
        , map
        , zip
        , zipWith
        , pack
        , filter
        , gather

        -- * Reduction
        , foldl
        , folds
        , sums)

where
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude  hiding (map, zip, zipWith, foldl, filter, replicate)


-- TODO: add a separate buffer function that converts the output
--       of pack back into a flow prepared to give four elements at a time.

-- TODO: add 'reflow', that evaluate elements into a buffer then 
--       converts back to a flow, for caching.

-- TODO: could write 'drop' function using an 'flowAdvance' field that
--       advances the flow without nessesarally computing the elements.

-- TODO: write 'take' to incrementally pull data.
--       take returns an unboxed vector of the given length
--       flow retuning take would just keep a second length
--       and push nothing after this length.

-- TODO: dup2 :: Flow a -> (Flow a, Flow a)
--       Creates two linked handles that buffer data until it has 
--       been pulled from both. Can still fuse into both consumers,
--       and only buffers the data that is required.
--       Doesn't force whole vector to be evaluated when we have sharing.
--       With higher degrees of duplication, might not to want to check
--       all consumers after every pull. Use a pull counter and only 
--       check for syncronisation after N pulls.

-- TODO: this should make it easy to write the parallel segmented
--       fold that exchanges data with its neighbours.

-- TODO: write recursive version of pack that buffers results.
--       until it gets enough to send a quad.

-- Think of situtions where we'll get desync between ends of dup2
-- Maybe with segmented fold, or pack / zip


-- | Flows provide a version of stream fusion that does not depend on the 
--   constructor specialisation transform, or strictness analysis working
--   particularly well.
-- 
--   Using the `flowGet4` interface, four elements of a flow are computed for
--   each loop interation, producing efficient object code.
data Flow a
        = Flow
        { -- | How many elements are available in this flow.
          flowSize      :: () -> IO Size

          -- | Takes a continuation and either calls it with `Just` an
          --   element or `Nothing` if no more elements are available.
        , flowGet1      :: (Maybe a -> IO ())
                        -> IO ()

          -- | Takes a continuation and either calls it with a `Left`
          --   8-tuple of elements or `Right` `Int` if less than eight
          --   elements are available.
          -- 
          --   The integer value indicates that there are at least this many
          --   elements still available.
        , flowGet8      :: (Either (a, a, a, a, a, a, a, a) Int -> IO ())
                        -> IO ()
        }

data Size
        = -- | Flow produces exactly this number of elements.
          Exact Int#

          -- | Flow produces at most this number of elements.
        | Max   Int#


-- | Take the minimum of two flow sizes.
sizeMin :: Size -> Size -> Size
sizeMin !s1 !s2
 = case (s1, s2) of
        (Exact len1, Exact len2)        -> Exact (min# len1 len2)
        (Exact len1, Max   len2)        -> Max   (min# len1 len2)
        (Max   len1, Exact len2)        -> Max   (min# len1 len2)
        (Max   len1, Max   len2)        -> Max   (min# len1 len2)
 where
        min# x1 x2 
         = if x1 <=# x2 then x1 else x2
        {-# INLINE min# #-}

{-# INLINE [0] sizeMin #-}


-------------------------------------------------------------------------------
-- | Convert an unboxed vector to a flow.
flow :: U.Unbox a => U.Vector a -> IO (Flow a)
flow !vec
 = do   -- Offset into the source vector.
        refIx   <- UM.unsafeNew 1
        UM.unsafeWrite refIx 0 0
        let !(I# len)    = U.length vec

        let 
         getSize _   
          = do  !(I# ix) <- UM.unsafeRead refIx 0
                return  $ Exact (len -# ix)
         {-# INLINE getSize #-}

         get1 push1
          = do  !(I# ix) <- UM.unsafeRead refIx 0
                if ix >=# len
                 then   push1 Nothing
                 else do
                        UM.unsafeWrite refIx 0 (I# (ix +# 1#))
                        push1 (Just (U.unsafeIndex vec (I# ix)))
         {-# INLINE get1 #-}

         get8 push8
          = do  !(I# ix) <- UM.unsafeRead refIx 0
                if ix +# 8# ># len
                 then   push8 $ Right 1
                 else do
                        UM.unsafeWrite refIx 0 (I# (ix +# 8#))
                        push8 $ Left    ( U.unsafeIndex vec (I# (ix +# 0#))
                                        , U.unsafeIndex vec (I# (ix +# 1#))
                                        , U.unsafeIndex vec (I# (ix +# 2#))
                                        , U.unsafeIndex vec (I# (ix +# 3#))
                                        , U.unsafeIndex vec (I# (ix +# 4#))
                                        , U.unsafeIndex vec (I# (ix +# 5#))
                                        , U.unsafeIndex vec (I# (ix +# 6#))
                                        , U.unsafeIndex vec (I# (ix +# 7#)))
         {-# INLINE get8 #-}

        return $ Flow getSize get1 get8
{-# INLINE [1] flow #-}


-------------------------------------------------------------------------------
-- | Fully evaluate a flow, producing an unboxed vector.
unflow :: U.Unbox a => Flow a -> IO (U.Vector a)
unflow !ff
 = do   size    <- flowSize ff ()
        case size of
         Exact len       -> unflowExact ff len
         Max   len       -> unflowExact ff len         -- TODO: this is wrong
{-# INLINE [1] unflow #-}


-- TODO: keep a mutable length in the flow.
--       set it to zero after we've pulled all the elements.
-- After taking just some elements, update the size.


unflowExact :: U.Unbox a => Flow a -> Int# -> IO (U.Vector a)
unflowExact ff !len
 = do   !mvec    <- UM.unsafeNew (I# len)
        !len'    <- slurp (UM.unsafeWrite mvec) ff
        !vec     <- U.unsafeFreeze mvec
        return   $  U.unsafeSlice 0 len' vec
{-# INLINE [1] unflowExact #-}


-- | Slurp out all the elements from a flow,
--   passing them to the provided consumption function.
slurp   :: (Int -> a -> IO ())
        -> Flow a
        -> IO Int

slurp !write ff
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 0

        let 
         slurp1 ix
          =  flowGet1 ff $ \r 
          -> case r of
                 Just x
                  -> do write (I# ix) x
                        slurp1 (ix +# 1#)

                 Nothing
                  ->    UM.unsafeWrite refCount 0 (I# ix)

         slurp8 ix 
          = flowGet8 ff $ \r 
          -> case r of
                Left (x0, x1, x2, x3, x4, x5, x6, x7)
                 -> do  write (I# (ix +# 0#)) x0
                        write (I# (ix +# 1#)) x1
                        write (I# (ix +# 2#)) x2
                        write (I# (ix +# 3#)) x3
                        write (I# (ix +# 4#)) x4
                        write (I# (ix +# 5#)) x5
                        write (I# (ix +# 6#)) x6
                        write (I# (ix +# 7#)) x7
                        slurp8 (ix +# 8#)

                Right !_
                 ->     slurp1 ix

        slurp8 0#
        UM.unsafeRead refCount 0
{-# INLINE [0] slurp #-}


------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying the function to each
--   index.
generate :: Int -> (Int -> a) -> IO (Flow a)
generate (I# len) f
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 0

        let
         getSize _
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                return  $ Exact (len -# ix)

         get1 push1
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                if ix >=# len
                 then    push1 Nothing
                 else do UM.unsafeWrite refCount 0 (I# (ix +# 1#))
                         push1 $ Just (f (I# ix))

         get8 push8
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                if ix +# 8# ># len
                 then   push8 $ Right 1
                 else do
                        UM.unsafeWrite refCount 0 (I# (ix +# 8#))
                        push8 $ Left    ( f (I# (ix +# 0#))
                                        , f (I# (ix +# 1#))
                                        , f (I# (ix +# 2#))
                                        , f (I# (ix +# 3#))
                                        , f (I# (ix +# 4#))
                                        , f (I# (ix +# 5#))
                                        , f (I# (ix +# 6#))
                                        , f (I# (ix +# 7#)))

        return $ Flow getSize get1 get8
{-# INLINE [1] generate #-}


-- | Produce an flow of the given length with the same value in each position.
replicate :: Int -> a -> IO (Flow a)
replicate n x
        = generate n (const x)
{-# INLINE [1] replicate #-}


-- | Yield a vector of the given length containing values @x@, @x+1@ etc.
enumFromN :: (U.Unbox a, Num a, Show a) => a -> Int -> IO (Flow a)
enumFromN start (I# len)
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 (I# len)

        refAcc   <- UM.unsafeNew 1
        UM.unsafeWrite refAcc   0 start

        let
         getSize _
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                return  $ Exact (len -# count)

         get1 push1
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count ==# 0#
                 then   push1 Nothing
                 else do 
                        UM.unsafeWrite refCount 0 (I# (count -# 1#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 1)

                        push1 $ Just acc

         get8 push8
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count <=# 8#
                 then   push8 (Right 1)
                 else do
                        UM.unsafeWrite refCount 0 (I# (count -# 8#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 8)

                        push8 $ Left    ( acc
                                        , acc + 1
                                        , acc + 2
                                        , acc + 3
                                        , acc + 4
                                        , acc + 5
                                        , acc + 6
                                        , acc + 7)

        return $ Flow getSize get1 get8
{-# INLINE [1] enumFromN #-}


------------------------------------------------------------------------------
-- | Apply a function to every element of a flow.
map :: (a -> b) -> Flow a -> Flow b
map f (Flow getSize get1 get8)
 = Flow getSize get1' get8'
 where  
        get1' push1
         =  get1 $ \r 
         -> case r of
                Just x  -> push1 $ Just (f x)
                Nothing -> push1 $ Nothing
        {-# INLINE get1' #-}

        get8' push8
         =  get8 $ \r
         -> case r of
                Left (x0, x1, x2, x3, x4, x5, x6, x7)
                 -> push8 $ Left (f x0, f x1, f x2, f x3, f x4, f x5, f x6, f x7)

                Right len
                 -> push8 $ Right len
        {-# INLINE get8' #-}

{-# INLINE [1] map #-}


-------------------------------------------------------------------------------
-- | Combine two flows into a flow of tuples.
zip :: Flow a -> Flow b -> Flow (a, b)
zip    (Flow !getSizeA getA1 getA8)
       (Flow !getSizeB getB1 getB8)
 = Flow getSize' get1' get8'
 where
        getSize' _
         = do   sizeA   <- getSizeA ()
                sizeB   <- getSizeB ()
                return  $  sizeMin sizeA sizeB
        {-# INLINE getSize' #-}

        get1' push1
         =  getA1 $ \mxA 
         -> getB1 $ \mxB
         -> case (mxA, mxB) of
                (Just xA, Just xB) -> push1 $ Just (xA, xB)
                _                  -> push1 $ Nothing
        {-# INLINE get1' #-}

        get8' push8
         =  getA8 $ \mxA
         -> getB8 $ \mxB
         -> case (mxA, mxB) of
                ( Left (a0, a1, a2, a3, a4, a5, a6, a7)
                 ,Left (b0, b1, b2, b3, b4, b5, b6, b7))
                 -> push8 $ Left  ( (a0, b0), (a1, b1), (a2, b2), (a3, b3)
                                  , (a4, b4), (a5, b5), (a6, b6), (a7, b7))

                ( Right len, _) 
                 -> push8 $ Right len

                ( _, Right len) 
                 -> push8 $ Right len
        {-# INLINE get8' #-}

{-# INLINE [1] zip #-}


-------------------------------------------------------------------------------
-- | Combine two flows with a function.
zipWith :: (a -> b -> c) -> Flow a -> Flow b -> Flow c
zipWith f flowA flowB
        = map (uncurry f) $ zip flowA flowB
{-# INLINE [1] zipWith #-}


-------------------------------------------------------------------------------
-- | Produce only the elements that have their corresponding flag set to `True`.
---  TODO: This can only produce elements one at a time.
--   Use a buffer instead to collect elements from the source.
pack :: Flow (Bool, a) -> Flow a
pack (Flow getSize get1 _)
 = Flow getSize' get1' get8'
 where
        getSize' _
         = do   size    <- getSize ()
                return  $ case size of
                           Exact len       -> Max len
                           Max   len       -> Max len

        get1' push1
         = eat ()
         where  eat ()
                 =  get1 $ \mx
                 -> case mx of
                        Just (True,  x) -> push1 (Just x)
                        Just (False, _) -> eat ()
                        Nothing         -> push1 Nothing

        get8' push8
         = push8 $ Right 1
{-# INLINE [1] pack #-}


-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: (a -> Bool) -> Flow a -> Flow a
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}


-------------------------------------------------------------------------------
-- | Takes a vector and a flow of indices, and produces a flow of elements
--   corresponding to each index.
gather :: U.Unbox a => U.Vector a -> Flow Int -> Flow a
gather !vec (Flow getSize get1 get8)
 = Flow getSize get1' get8'
 where
        get1' push1
         =  get1 $ \r
         -> case r of
                Just ix 
                 -> push1 $ Just (U.unsafeIndex vec ix) 

                Nothing
                 -> push1 $ Nothing
        {-# INLINE get1' #-}

        get8' push8
         =  get8 $ \r
         -> case r of
                Left (ix0, ix1, ix2, ix3, ix4, ix5, ix6, ix7)
                 -> push8 $ Left ( U.unsafeIndex vec ix0
                                 , U.unsafeIndex vec ix1
                                 , U.unsafeIndex vec ix2
                                 , U.unsafeIndex vec ix3
                                 , U.unsafeIndex vec ix4
                                 , U.unsafeIndex vec ix5
                                 , U.unsafeIndex vec ix6
                                 , U.unsafeIndex vec ix7 )

                Right len
                 -> push8 $ Right len
        {-# INLINE get8' #-}

{-# INLINE [1] gather #-}


-------------------------------------------------------------------------------
-- | Fold Left. Reduce a flow to a single value.
foldl :: U.Unbox a => (a -> b -> a) -> a -> Flow b -> IO a
foldl f z !(Flow _ get1 get8)
 = do   outRef  <- UM.unsafeNew 1
        UM.unsafeWrite outRef 0 z

        let 
         eat1 !acc
          = get1 $ \r 
          -> case r of
                Just x1
                 -> eat1 (acc `f` x1)

                Nothing
                 -> UM.write outRef 0 acc

         eat8 !acc 
          =  get8 $ \r 
          -> case r of
                Left (x0, x1, x2, x3, x4, x5, x6, x7)
                  -> eat8 ( acc `f` x0 `f` x1 `f` x2 `f` x3
                                `f` x4 `f` x5 `f` x6 `f` x7) 

                Right _
                  -> eat1 acc

        eat8 z
        UM.read outRef 0
{-# INLINE [1] foldl #-}


-------------------------------------------------------------------------------
-- | Fold Segmented. Takes a flow of segment lengths and a flow of elements,
--   and reduces each segment to a value individually.
folds :: U.Unbox a => (a -> b -> a) -> a -> Flow Int -> Flow b -> Flow a
folds f !z (Flow getSize getLen1 _) (Flow !_ getElem1 getElem8)
 = Flow getSize get1' get8'
 where
        -- Pull the length of the first segment.
        get1' push1
         =  getLen1 $ \r
         -> case r of 
                Just (I# len)   -> foldSeg push1 len
                Nothing         -> push1 Nothing
        {-# INLINE get1' #-}

        -- Producing eight copies of the loop that folds a segment won't make
        -- anything faster, so tell the consumer they can only pull one result
        -- at a time.
        get8' push8
         = push8 $ Right 1
        {-# INLINE get8' #-}

        -- Fold a single segment
        --  Start out pulling four elements at a time, then switch to
        --  one-at-a-time when we get close to the end of the segment.
        --  We don't want to pull any elements from the _next_ segment
        --  because then we'd have to stash them until the next result
        --  was pulled from us.
        foldSeg push1 !len
         = go8 len z
         where  
                go8 n  !acc
                 -- If there are less than a whole packet of elements
                 -- reamaining them switch to pulling them one at a time.
                 |  n <# 8#
                 =  go1 n acc

                 -- There are at least eight elements remaining, 
                 -- so we can do an unrolled fold.
                 |  otherwise
                 =  getElem8  $ \r
                 -> case r of
                        Left (x0, x1, x2, x3, x4, x5, x6, x7)
                         -> let !acc' = acc `f` x0 `f` x1 `f` x2 `f` x3 
                                            `f` x4 `f` x5 `f` x6 `f` x7
                            in  go8 (n -# 8#) acc'

                        Right _
                         -> go1 n acc

                -- We've reached the end of the segment.
                -- Push the final result to the consumer.
                go1 0# !acc
                 = push1 $ Just acc

                -- Less than four elements remaining in the segment,
                -- fold the rest one at a time.
                go1 n  !acc
                 =  getElem1 $ \r
                 -> case r of
                        Just x1
                         -> let !acc' = acc `f` x1
                            in   go1 (n -# 1#) acc'

                        Nothing 
                         -> go1 0# acc 
                         -- error "folds: not enough elements for segment."
                         --  If we hit this case then the segment lengths 
                         --  don't match the data we have, but just return the
                         --  current acc to avoid producing code for the error.
                         --  (yeah, you heard me)
        {-# INLINE foldSeg #-}
{-# INLINE [1] folds #-}


-- | Sum Segmented. Takes a flow of segment lenghths and a flow of elements,
--   and sums the elements of each segment individually.
sums :: (U.Unbox a, Num a) => Flow Int -> Flow a -> Flow a
sums ffLens ffElems
        = folds (+) 0 ffLens ffElems
{-# INLINE [1] sums #-}


-- Other operators ------------------------------------------------------------
--
-- Do we need this in addition to zip?
--  link2   :: Flow a 
--         -> (Flow a -> Flow b)
--         -> (Flow a -> Flow c)
--         -> (Flow (b, c))

-- Unzip is dup2 followed by map snd / map fst
--  unzip2  :: Flow (a, b)
--          -> (Flow a, Flow b)

