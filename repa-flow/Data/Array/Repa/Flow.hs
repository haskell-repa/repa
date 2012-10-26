{-# LANGUAGE MagicHash #-}
module Data.Array.Repa.Flow
        ( Flow (..)
        , flow
        , unflow
        , map)
where
import Data.IORef
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude  hiding (map)


--  With perfect stream fusion we'll end up with a single loop that
--  passes many state variables back to itself.
--
--  Instead, keep state in mutable variables local to each function, 
--  and process multiple elements at a time to amortise the cost of
--  reading and writing the state variables. The state will be stored
--  in L1 cache, so reading and writing the state shouldn't cost more
--  than spilling a register to the stack.
--
--  When we have multiple consumers for a flow we'll have to use a 
--  combinator the caches the current packet read from the source.
--
--  With zipWith, the cost of checking for the end-of-stream won't 
--  be as high if we pull several elements at once.
--

data Flow a
        = Flow
        { flowSize      :: Size

          -- | Return either a single element from a flow,
          --   or Nothing if there are no more elements.
        , flowPull1     :: () -> IO (Maybe a)

          -- | Returns either a quad of elements or the number of elements
          --   remaining if there are less than four.
        , flowPull4     :: () -> IO (Either (a, a, a, a) Int) }

data Size
        = Exact Int




-- map ------------------------------------------------------------------------
map :: (a -> b) -> Flow a -> Flow b
map f (Flow size pull1 pull4)
 = Flow size pull1' pull4'
 where  
        pull1' ()
         = do   mx       <- pull1 ()
                case mx of
                 Just x  
                  -> return $ Just $ f x

                 Nothing 
                  -> return Nothing

        pull4' ()
         = do   mx      <- pull4 ()
                case mx of
                 Left (x1, x2, x3, x4)  
                  -> return $ Left (f x1, f x2, f x3, f x4)

                 Right len
                  -> return $ Right len


-- flow -----------------------------------------------------------------------
-- | Convert an unboxed vector to a flow.
flow :: U.Unbox a => U.Vector a -> IO (Flow a)
flow vec
 = newIORef 0 >>= \refIx
 -> let 
        pull1 ()
         = do   ix      <- readIORef refIx
                if ix >= U.length vec
                 then   return Nothing
                 else do
                        writeIORef refIx (ix + 1)
                        return  $ Just (U.unsafeIndex vec ix)

        pull4 ()
         = do   ix      <- readIORef refIx
                let len = U.length vec
                if ix + 4 >= len
                 then   return  $ Right (len - ix)
                 else do
                        writeIORef refIx (ix + 4)
                        return  $ Left  ( U.unsafeIndex vec ix
                                        , U.unsafeIndex vec (ix + 1)
                                        , U.unsafeIndex vec (ix + 2)
                                        , U.unsafeIndex vec (ix + 3) )

   in   return 
         $ Flow (Exact (U.length vec))
                pull1
                pull4
{-# INLINE [1] flow #-}


-- unflow ---------------------------------------------------------------------
-- | Fully evaluate a flow, producing an unboxed vector.
unflow :: U.Unbox a => Flow a -> IO (U.Vector a)
unflow flow
 = case flowSize flow of
        Exact len       -> unflowExact flow len
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
         = do   r       <- flowPull1 flow ()
                case r of
                 Just x
                  -> do write (I# ix) x
                        slurp1 (ix +# 1#)

                 Nothing
                  -> return ()

        slurp4 ix 
         = do   r       <- flowPull4 flow ()
                case r of
                 Left (x1, x2, x3, x4)
                  -> do write (I# ix)         x1
                        write (I# (ix +# 1#)) x2
                        write (I# (ix +# 2#)) x3
                        write (I# (ix +# 3#)) x4
                        slurp4 (ix +# 4#)

                 Right n
                  ->    slurp1 ix
{-# INLINE [0] slurp #-}

