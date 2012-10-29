
module Data.Array.Repa.Flow.Base
        ( Flow(..)
        , Size(..)
        , sizeMin
        , flow
        , unflow)
where
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM


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
