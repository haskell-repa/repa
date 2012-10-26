{-# LANGUAGE MagicHash, BangPatterns #-}
module Data.Array.Repa.Flow
        ( Flow (..)
        , flow
        , unflow
        , map
        , zip
        , zipWith
        , foldl
        , indexs
        , pack
        , filter)
where
import Data.IORef
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude  hiding (map, zip, zipWith, foldl, filter)


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
{-# INLINE [1] map #-}


-- zip ------------------------------------------------------------------------
zip :: Flow a -> Flow b -> Flow (a, b)
zip     (Flow sizeA pullA1 pullA4)
        (Flow sizeB pullB1 pullB4)
 = Flow (sizeMin sizeA sizeB) pull1' pull4'
 where
        pull1' ()
         = do   !mxA     <- pullA1 ()
                !mxB     <- pullB1 ()
                case (mxA, mxB) of
                 (Just xA, Just xB)
                   -> return $ Just $ (xA, xB)

                 _ -> return $ Nothing

        pull4' ()
         = do   !mxA     <- pullA4 ()
                !mxB     <- pullB4 ()
                case (mxA, mxB) of
                 (  Left (a1, a2, a3, a4)
                  , Left (b1, b2, b3, b4))
                  -> return $ Left  ((a1, b1), (a2, b2), (a3, b3), (a4, b4))

                 (Right len, _) 
                  -> return $ Right len

                 (_, Right len) 
                  -> return $ Right len
{-# INLINE [1] zip #-}


-- zipWith --------------------------------------------------------------------
zipWith :: (a -> b -> c) -> Flow a -> Flow b -> Flow c
zipWith f flowA flowB
        = map (uncurry f) $ zip flowA flowB
{-# INLINE [1] zipWith #-}


-- foldl ----------------------------------------------------------------------
foldl :: (a -> b -> a) -> a -> Flow b -> IO a
foldl f z (Flow size pull1 pull4)
 = eat4 z
 where
        eat1 !acc
         = do   !mx     <- pull1 ()
                case mx of
                 Just x1
                  -> eat1 (acc `f` x1)

                 Nothing
                  -> return acc

        eat4 !acc 
         = do   !mx     <- pull4 ()
                case mx of
                 Left (x1, x2, x3, x4)
                  -> eat4 (acc `f` x1 `f` x2 `f` x3 `f` x4)

                 Right _
                  -> eat1 acc
{-# INLINE [1] foldl #-}


-- indexs ---------------------------------------------------------------------
indexs :: U.Unbox a => U.Vector a -> Flow Int -> Flow a
indexs vec (Flow size pull1 pull4)
 = Flow size pull1' pull4'
 where
        pull1' ()
         = do   !mix    <- pull1 ()
                case mix of
                 Just ix        
                  -> return $ Just (U.unsafeIndex vec ix)

                 Nothing        
                  -> return $ Nothing

        pull4' ()
         = do   !mix    <- pull4 ()
                case mix of
                 Left (ix1, ix2, ix3, ix4)
                  -> return $ Left ( U.unsafeIndex vec ix1
                                   , U.unsafeIndex vec ix2
                                   , U.unsafeIndex vec ix3
                                   , U.unsafeIndex vec ix4)

                 Right len
                  -> return $ Right len
{-# INLINE [1] indexs #-}


-- pack -----------------------------------------------------------------------
pack :: U.Unbox a => Flow (Bool, a) -> IO (Flow a)
pack (Flow size pull1 pull4)
 = newIORef Stash0 >>= \refStash
 -> let 
        size'
         = case size of
                Exact len       -> Max len
                Max   len       -> Max len

        pull1' ()
         = do   !mx   <- pull1 ()
                case mx of
                 Just (True,  x) -> return $ Just x
                 Just (False, _) -> pull1' ()
                 Nothing         -> return $ Nothing

        pull4' ()
         = do   return $ Right 1

    in  return $ Flow size pull1' pull4'

{-# INLINE [1] pack #-}


-- filter ---------------------------------------------------------------------
filter :: U.Unbox a => (a -> Bool) -> Flow a -> IO (Flow a)
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}


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
        Max   len       -> unflowExact flow len         -- wrong
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

