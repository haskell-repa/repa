{-# LANGUAGE MagicHash, BangPatterns, ExistentialQuantification #-}
module Data.Array.Repa.CoFlow
        ( Flow (..) 
        , flow
        , unflow
        , map
        , pack
        , filter
        , foldl)
where
import Data.IORef
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude  hiding (map, zip, zipWith, foldl, filter)


-- TODO: write a separate buffer function that converts the output
--       of pack back into a flow prepared to give four elements at a time.

data Flow a
        = Flow
        { flowSize      :: Size

          -- | Takes a continuation and either calls it with Just an element
          --   or Nothing if no more elements are available.
        , flowGet1      :: (Maybe a -> IO ())
                        -> IO ()

          -- | Takes a continuation and either calls it with a Left 4-tuple
          --   of elements or Right Int if less than four elements
          --   are available.
        , flowGet4      :: (Either (a, a, a, a) Int -> IO ())
                        -> IO ()
        }

data Size
        = Exact Int
        | Max   Int

sizeMin :: Size -> Size -> Size
sizeMin s1 s2
 = case (s1, s2) of
        (Exact len1, Exact len2)        -> Exact (min len1 len2)
        (Exact len1, Max   len2)        -> Max   (min len1 len2)
        (Max   len1, Exact len2)        -> Max   (min len1 len2)
{-# INLINE [0] sizeMin #-}


-- map ------------------------------------------------------------------------
map :: (a -> b) -> Flow a -> Flow b
map f (Flow size get1 get4)
 = Flow size get1' get4'
 where  
        get1' push1
         =  get1 $ \r 
         -> case r of
                Just x  -> push1 $ Just (f x)
                Nothing -> push1 $ Nothing

        get4' push4
         =  get4 $ \r
         -> case r of
                Left (x1, x2, x3, x4)
                 -> push4 $ Left (f x1, f x2, f x3, f x4)

                Right len
                 -> push4 $ Right len
{-# INLINE [1] map #-}


-- pack -----------------------------------------------------------------------
pack :: U.Unbox a => Flow (Bool, a) -> Flow a
pack (Flow size get1 get4)
 = Flow size get1' get4'
 where
        size'
         = case size of
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

        get4' push4
         = push4 $ Right 1
{-# INLINE [1] pack #-}


-- filter ---------------------------------------------------------------------
filter :: U.Unbox a => (a -> Bool) -> Flow a -> Flow a
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}


-- foldl ----------------------------------------------------------------------
foldl :: (a -> b -> a) -> a -> Flow b -> IO a
foldl f z (Flow size get1 get4)
 =  newIORef z >>= \outRef
 -> let 
        eat1 !acc
         = get1 $ \r 
         -> case r of
                Just x1
                 -> eat1 (acc `f` x1)

                Nothing
                 -> writeIORef outRef acc

        eat4 !acc 
         =  get4 $ \r 
         -> case r of
                Left (x1, x2, x3, x4)
                  -> eat4 (acc `f` x1 `f` x2 `f` x3 `f` x4)

                Right _
                  -> eat1 acc

    in do
        eat4 z
        readIORef outRef

{-# INLINE [1] foldl #-}


-- flow -----------------------------------------------------------------------
-- | Convert an unboxed vector to a flow.
flow :: U.Unbox a => U.Vector a -> IO (Flow a)
flow vec
 = newIORef 0 >>= \refIx
 -> let 
        get1 push1
         = do   ix      <- readIORef refIx
                if ix >= U.length vec
                 then   push1 Nothing
                 else do
                        writeIORef refIx (ix + 1)
                        push1 (Just (U.unsafeIndex vec ix))

        get4 push4
         = do   ix      <- readIORef refIx
                let len = U.length vec
                if ix + 4 >= len
                 then   push4 $ Right (len - ix)
                 else do
                        writeIORef refIx (ix + 4)
                        push4 $ Left  ( U.unsafeIndex vec ix
                                      , U.unsafeIndex vec (ix + 1)
                                      , U.unsafeIndex vec (ix + 2)
                                      , U.unsafeIndex vec (ix + 3) )

   in   return 
         $ Flow (Exact (U.length vec)) get1 get4
{-# INLINE [1] flow #-}


-- unflow ---------------------------------------------------------------------
-- | Fully evaluate a flow, producing an unboxed vector.
unflow :: U.Unbox a => Flow a -> IO (U.Vector a)
unflow flow
 = case flowSize flow of
        Exact len       -> unflowExact flow len
        Max   len       -> unflowExact flow len         -- TODO: this is wrong
{-# INLINE [1] unflow #-}


unflowExact :: U.Unbox a => Flow a -> Int -> IO (U.Vector a)
unflowExact flow len
 = do   mvec    <- UM.unsafeNew len
        slurp (UM.unsafeWrite mvec) flow
        U.unsafeFreeze mvec
{-# INLINE [1] unflowExact #-}


-- | Slurp out all the elements from a flow,
--   passing them to the provided consumption function.
slurp   :: (Int -> a -> IO ())
        -> Flow a
        -> IO ()

slurp write flow
 = slurp4 0#
 where  
        slurp1 ix
         =  flowGet1 flow $ \r 
         -> case r of
                 Just x
                  -> do write (I# ix) x
                        slurp1 (ix +# 1#)

                 Nothing
                  -> return ()

        slurp4 ix 
         = flowGet4 flow $ \r 
         -> case r of
                Left (x1, x2, x3, x4)
                 -> do  write (I# ix)         x1
                        write (I# (ix +# 1#)) x2
                        write (I# (ix +# 2#)) x3
                        write (I# (ix +# 3#)) x4
                        slurp4 (ix +# 4#)

                Right n
                 ->     slurp1 ix
{-# INLINE [0] slurp #-}

