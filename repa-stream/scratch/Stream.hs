{-# LANGUAGE ExistentialQuantification, MagicHash, BangPatterns, ScopedTypeVariables #-}
module Stream where
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Concurrent.MVar
import Control.Monad.ST
import Control.Monad
import GHC.Exts

-- 
data Stream m a
        = forall s
        . Stream 
        { streamSize    :: Size
        , streamStart   :: s
        , streamNext    :: s -> m (Step s a)
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


-- Stream ---------------------------------------------------------------------
-- | Construct a stream.
stream  :: Monad m
        => Int#                 -- ^ Overall size of the stream.
        -> (Int# -> a)          -- ^ Get the element at this position.
        -> Stream m a

stream size get
 = Stream (Exact size) 0 mkStep
 where  mkStep (I# ix)
         | ix >=# size  = return $ Done
         | otherwise    = return $ Yield (I# (ix +# 1#)) (get ix)
        {-# INLINE [0] mkStep #-}
{-# INLINE [1] stream #-}


-- Map ------------------------------------------------------------------------
-- | Apply a function to every element of a stream.
map     :: Monad m 
        => (a -> b) -> Stream m a -> Stream m b
map f (Stream len start next)
 = Stream len start next'
 where  next' s
         =  next s >>= \p 
         -> case p of
                Yield  s' x     -> return $ Yield  s' (f x)
                Update s'       -> return $ Update s'
                Done            -> return $ Done
{-# INLINE [1] map #-}


-- foldSegs -------------------------------------------------------------------
foldSegs :: Monad m
         => (a -> b -> a)        -- ^ Function to perform the fold.
         -> a                    -- ^ Initial element of each fold.
         -> Stream m Int         -- ^ Stream of segment lengths.
         -> Stream m b           -- ^ Stream of input data.
         -> Stream m a           -- ^ Stream of fold results.
        
foldSegs f z (Stream sz ss0 nexts) (Stream _ vs0 nextv) 
 = Stream sz (Nothing, z, ss0, vs0) next
 where
        {-# INLINE next #-}
        next (Nothing,x,ss,vs) 
         = nexts ss >>= \p
         -> case p of
             Done         -> return Done
             Update ss'   -> return $ Update (Nothing,x, ss', vs)
             Yield  ss' n -> return $ Update (Just n, z, ss', vs)

        next (Just 0,x,ss,vs) 
         = return $ Yield (Nothing,z,ss,vs) x

        next (Just n,x,ss,vs) 
         =  nextv vs >>= \p
         -> case p of
             Done         -> return Done -- NEVER ENTERED (See Note)
             Update vs'   -> return $ Update (Just n,x,ss,vs')
             Yield  vs' y -> return $ let r = f x y
                                      in  r `seq` (Update (Just (n-1), r, ss, vs'))
{-# INLINE foldSegs #-}


-- foldSegsTrade --------------------------------------------------------------
data FoldState s1 s2 a
        = FoldBegin
        { stateSource1   :: !s1
        , stateSource2   :: !s2 }

        | FoldSeg
        { stateSource1   :: !s1
        , stateSource2   :: !s2
        , stateSegIx     :: Int#          -- ^ What segment number we're currently in.
        , stateSegRemain :: Int# 
        , stateAcc       :: !a }

        | FoldEnd                         -- ^ We've already finished all the segments.

-- foldSegsTrade
foldSegsTrade 
        :: forall a
        .  (a -> a -> a)        -- ^ Function to perform the fold.
        -> a                    -- ^ Initial element of each fold.
        -> Stream IO Int        -- ^ Stream of segment lengths.
        -> Stream IO a          -- ^ Stream of input data.
        -> Maybe (MVar a)       -- ^ Write result of first seg to this var instead.
        -> Maybe (MVar a)       -- ^ Read  last element from this var instead.
        -> Stream IO a          -- ^ Stream of fold results.
        
foldSegsTrade 
        f z 
        (Stream sz ss0 nexts) 
        (Stream _  vs0 nextv) 
        vLeft vRight
 = Stream sz (FoldBegin ss0 vs0) next
 where
        -- Read the very first segment length from the stream.
        next (FoldBegin ss sv)
         =  nexts ss >>= \p
         -> case p of
             Done              -> return Done
             Update ss'        -> return $ Update (FoldBegin ss' sv)
             Yield  ss' (I# n) -> return $ Update (FoldSeg   ss' sv 0# n z)

        -- Get a new segment length after finishing a previous one.
        next (FoldSeg ss sv segIx 0# acc)
         =  nexts ss >>= \p
         -> case p of
             Done              -> sendLast acc
             Update ss'        -> return $ Update (FoldSeg ss' sv segIx         0# acc)
             Yield  ss' (I# n) -> send segIx      (FoldSeg ss' sv (segIx +# 1#) n  z)   acc

        -- Folding a segment.
        next (FoldSeg ss sv segIx n acc)
         =  nextv sv >>= \p
         -> case p of
             Done              -> return $ Done        -- never entered
             Update sv'        -> return $ Update (FoldSeg ss sv' n segIx acc)
             Yield  sv' x      -> let r = f acc x 
                                  in  r `seq` (return $ Update (FoldSeg ss sv' segIx (n -# 1#) r))

        -- We've finished folding everything.
        next FoldEnd
         = return Done
        {-# INLINE [0] next #-}

        -- Send a folded result to the consumer.
        send !0# !s !x
         = case vLeft of
             Nothing 
              ->     return $ Yield s x
             Just var
              -> do  putMVar var x
                     return $ Update s

        send _ s x
                = return $ Yield s x
        {-# INLINE [0] send #-}

        -- Send the very last folded result to a consumer.
        sendLast !x
         = case vRight of
            Nothing 
             ->     return $ Yield FoldEnd x

            Just var
             -> do  x2      <- readMVar var
                    let !x'  =  f x x2
                    return  $ Yield FoldEnd x'
        {-# INLINE [0] sendLast #-}

{-# INLINE [1] foldSegsTrade #-}


-- foldSegsBubble -------------------------------------------------------------


foldSegsBubble 
        :: (a -> a -> a)        -- ^ Function to perform the fold.
        -> a                    -- ^ Initial element of each fold.
        -> Stream IO Int         -- ^ Stream of segment lengths.
        -> Stream IO a           -- ^ Stream of input data.
        -> Maybe (MVar a)       -- ^ Write result of first seg to this var.
        -> Maybe (MVar a)       -- ^ Read  partial result for last seg from this var.
        -> Stream IO a           -- ^ Stream of fold results.

foldSegsBubble f z 
        (Stream sz ss0 nexts) 
        (Stream _  vs0 nextv) 
        mvLeft
        mvRight
 = Stream sz (0 :: Int, ss0, vs0) next
 where
        next (segId, ss, vs)
         =  nexts ss >>= \p
         -> case p of
                Done             
                 -> return Done

                 -- only hack the last segment
                        -- !x'      <- case mvRight of
                        --             Just vRight 
                        --              -> do  y  <- readMVar vRight 
                        --                     return $ f x y

                        --             Nothing
                        --              -> return x

                Update ss'       
                 -> return $ Update (segId, ss', vs)

                Yield  ss' n    
                 -> do  (vs', x) <- foldSeg n vs


                        case mvLeft of
                          Just vLeft
                           |     segId == 0 
                           -> do putMVar vLeft x
                                 return $ Update (segId + 1, ss', vs')

                          _ ->   return $ Yield  (segId + 1, ss', vs') x
        {-# INLINE [0] next #-}

        -- Use a local recursive function to consume the entire segment.
        foldSeg !len vs1
         = go len vs1 z
         where  go 0 vs !acc
                 = return (vs, acc)

                go n vs !acc
                 =  nextv vs >>= \p
                 -> case p of
                        Done         -> return (vs, acc)
                        Update vs'   -> go n       vs' acc
                        Yield  vs' x -> go (n - 1) vs' (f acc x)
        {-# INLINE [0] foldSeg #-}

{-# INLINE [1] foldSegsBubble #-}


-- Eval -----------------------------------------------------------------------
-- | Evaluate a stream,
--   calling the given function with the index and value of each element.
evalM   :: Monad m
        => (Int# -> a -> m ())
        -> Stream m a
        -> m Int

evalM write (Stream _size s0 next)
 = go 0# s0
 where  go ix !s
         = next s >>= \p
         -> case p of
                Yield !s' x
                 -> do  write ix x
                        go (ix +# 1#) s'

                Update !s'
                 ->     go ix s'

                Done
                 ->     return (I# ix)
{-# INLINE [1] evalM #-}


-- Unboxed --------------------------------------------------------------------
-- | Convert an unboxed vector to a stream.
streamUnboxed :: (Monad m, U.Unbox a) => U.Vector a -> Stream m a
streamUnboxed vec
 = let  !(I# len)       = U.length vec
        get ix          = vec `U.unsafeIndex` (I# ix)
   in   stream len get
{-# INLINE [1] streamUnboxed #-}


-- | Evaluate a stream, returning the elements in an unboxed vector.
unstreamUnboxed :: U.Unbox a => Stream IO a -> IO (U.Vector a)
unstreamUnboxed s@(Stream size _ _)
 = case size of
        Exact len       -> unstreamUnboxed_exact len    s
        Max   lenMax    -> unstreamUnboxed_max   lenMax s
        Unknown         -> error "unstreamUnboxed: unknown streams not finished"
{-# INLINE unstreamUnboxed #-}

unstreamUnboxed_exact len s
 = do   !mvec   <- UM.unsafeNew (I# len)

        let write ix x
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalM write s
        U.unsafeFreeze mvec
{-# INLINE unstreamUnboxed_exact #-}

unstreamUnboxed_max lenMax s
 = do   !mvec   <- UM.unsafeNew (I# lenMax)

        let write ix x
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        len     <- evalM write s
        vec     <- U.unsafeFreeze mvec
        return  $ U.slice 0 len vec
{-# INLINE unstreamUnboxed_max #-}
