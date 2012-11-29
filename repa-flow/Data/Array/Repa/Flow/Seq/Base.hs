

-- | Flows provide an incremental version of array fusion that allows the
--   the computation to be suspended and resumed at a later time.
module Data.Array.Repa.Flow.Seq.Base
        ( FD, FS
        , Flow(..)
        , Step1(..)
        , Step8(..)
        , Size(..)
        , sizeMin
        , flow
        , unflow
        , take
        , drain
        , slurp
        , Touch(..)

        -- Shorthands
        , unew
        , uread
        , uwrite

        , inew
        , iread
        , iwrite)
where
import GHC.Exts
import GHC.Types
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import System.IO.Unsafe
import Prelude                                          hiding (take)

-- | Phantom type tag to indicate a delayed flow.
--
--   A delayed flow is a flow that hasn't started flowing yet.
--   It does not have any attached state, and we can evaluate it multiple
--   times to get the same result.
--
data FD


-- | Phantom type tag to indicate a stateful flow.
--      
--   A stateful flow is a flow that has already started flowing.
--   It has attached state, and we can evaluated prefixes of it
--   incrementally. Evaluating the whole flow uses it up, 
--   so we can't evaluate it again.
--
data FS


-- | Flows provide an incremental version of array fusion that allows the
--   the computation to be suspended and resumed at a later time.
-- 
--   Using the `flowGet8` interface, eight elements of a flow can be 
--   computed for each loop iteration, producing efficient object code.
data Flow r a
        = forall state. Flow
        { 
          -- | Start the flow. 
          --   This returns a state value that needs to be passed to
          --   the get functions.
          flowStart     :: IO state

          -- | How many elements are available in this flow.
        , flowSize      :: state -> IO Size

          -- | Report the current state of this flow.
        , flowReport    :: state -> IO R.Report

          -- | Takes a continuation and calls it with
          --   a `Step1` containing some data.
        , flowGet1      :: state -> (Step1 a -> IO ()) -> IO ()

          -- | Takes a continuation and calls it with 
          --  a `Step8` containing some data.
        , flowGet8      :: state -> (Step8 a -> IO ()) -> IO ()
        }


data Step1 a
        -- | An element and a flag saying whether a full 8 elements are
        --   likely to be available next pull.
        --
        ---  We don't want to *force* the consumer to pull the full 8
        --   if it doesn't want to, otherwise functions like folds would
        --   become too complicated.
        = Yield1 a Bool

        -- | The flow is finished, no more elements are avaliable.
        | Done


data Step8 a
        -- | Eight successive elements of the flow.
        = Yield8 a a a a a a a a

        -- | Indicates that the flow cannot yield a full 8 elements right now.
        --   You should use `flowGet1` to get the next element and try `flowGet8` again later.
        | Pull1


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
-- | Convert an unboxed vector to a delayed flow.
flow :: (Touch a, U.Unbox a) => U.Vector a -> Flow r a
flow !vec
 = Flow start size report get1 get8
 where  
        start
         = do   refIx   <- inew 1
                iwrite refIx 0# 0#
                return refIx
        {-# INLINE start #-}


        size refIx
         = do   let !(I# len)   = U.length vec
                !(I# ix)        <- iread refIx 0#
                return  $ Exact (len -# ix)
        {-# INLINE size #-}


        report refIx
         = do   let !len        = U.length vec
                !ix             <- iread refIx 0#
                return  $ R.Flow len ix
        {-# NOINLINE report #-}


        get1 refIx push1
         = do   let !(I# len)    = U.length vec
                !(I# ix)        <- iread refIx 0#
                let !remain     =  len -# ix
                if remain ># 0#
                 then do
                        iwrite refIx 0# (ix +# 1#)
                        let !x  = U.unsafeIndex vec (I# ix)

                        -- Touch because we want to be sure its unboxed as
                        -- soon as we read it. It we don't touch it, and
                        -- the continuation uses the value in multiple
                        -- case branches then it can be reboxed and then
                        -- unboxed again multiple times.
                        touch x

                        push1 $ Yield1 x (remain >=# 9#)

                 else   push1 Done
        {-# INLINE get1 #-}


        get8 refIx push8
         = do   let !(I# len)   = U.length vec
                !(I# ix)        <- iread refIx 0#
                let !remain     = len -# ix
                if remain >=# 8#
                 then do
                        iwrite refIx 0# (ix +# 8#)

                        -- TODO: not sure whether we should force these here
                        let here' = return

                        !x0     <- here' $ U.unsafeIndex vec (I# (ix +# 0#))
                        !x1     <- here' $ U.unsafeIndex vec (I# (ix +# 1#))
                        !x2     <- here' $ U.unsafeIndex vec (I# (ix +# 2#))
                        !x3     <- here' $ U.unsafeIndex vec (I# (ix +# 3#))
                        !x4     <- here' $ U.unsafeIndex vec (I# (ix +# 4#))
                        !x5     <- here' $ U.unsafeIndex vec (I# (ix +# 5#))
                        !x6     <- here' $ U.unsafeIndex vec (I# (ix +# 6#))
                        !x7     <- here' $ U.unsafeIndex vec (I# (ix +# 7#))

                        push8 $ Yield8 x0 x1 x2 x3 x4 x5 x6 x7

                 else do
                        push8 Pull1
        {-# INLINE get8 #-}

{-# INLINE [1] flow #-}


-------------------------------------------------------------------------------
-- | Fully evaluate a delayed flow, producing an unboxed vector.
--
unflow :: (Touch a, U.Unbox a) => Flow FD a -> U.Vector a
unflow ff = unsafePerformIO $ drain ff
{-# INLINE [1] unflow #-}


-- | Fully evaluate a possibly stateful flow,
--   pulling all the remaining elements.
drain :: (Touch a, U.Unbox a) => Flow r a -> IO (U.Vector a)
drain !ff
 = case ff of
    Flow fStart fSize _ fGet1 fGet8
     -> do !state   <- fStart
           !size    <- fSize state 

           -- In the sequential case we can use the same code to unflow
           -- both Exact and Max, and just slice down the vector to the
           -- final size.
           case size of
            Exact len       -> unflowWith len (fGet1 state) (fGet8 state)
            Max   len       -> unflowWith len (fGet1 state) (fGet8 state)
{-# INLINE [1] drain #-}


unflowWith
        :: (Touch a, U.Unbox a) 
        => Int# 
        -> ((Step1 a -> IO ()) -> IO ())
        -> ((Step8 a -> IO ()) -> IO ())
        -> IO (U.Vector a)

unflowWith !len get1 get8
 = do   !mvec    <- UM.unsafeNew (I# len)
        !len'    <- slurp 0# Nothing (UM.unsafeWrite mvec) get1 get8
        !vec     <- U.unsafeFreeze mvec
        return   $  U.unsafeSlice 0 len' vec
{-# INLINE [1] unflowWith #-}


-------------------------------------------------------------------------------
-- | Take the given number of elements from the front of a flow,
--   returning those elements and the rest of the flow.
--   
--   Calling 'take' allocates buffers and other state information, 
--   and this state will be reused when the remaining elements of
--   the flow are evaluated.
--
take    :: (Touch a, U.Unbox a) 
        => Int# -> Flow r a -> IO (U.Vector a, Flow FS a)

take limit (Flow start size report get1 get8)
 = do   state    <- start

        !mvec    <- UM.unsafeNew (I# limit)
        !len'    <- slurp 0# (Just (I# limit)) (UM.unsafeWrite mvec) 
                        (get1 state) (get8 state)
        !vec     <- U.unsafeFreeze mvec
        let !vec' = U.unsafeSlice 0 len' vec        

        return  ( vec'
                , Flow (return state) size report get1 get8)

{-# INLINE [1] take #-}


-------------------------------------------------------------------------------
-- | Slurp out all the elements from a flow,
--   passing them to the provided consumption function.
slurp   :: Touch a
        => Int#
        -> Maybe Int
        -> (Int -> a -> IO ())
        -> ((Step1 a  -> IO ()) -> IO ())
        -> ((Step8 a  -> IO ()) -> IO ())
        -> IO Int

slurp start stop !write get1 get8
 = do   refCount <- inew 1
        iwrite refCount 0# (-1#)

        let
         slurpSome ix
          = do  slurp8 ix
                I# ix'     <- iread refCount 0# 

                slurp1 ix' 
                I# ix''    <- iread refCount 0#

                case stop of
                 Just (I# limit)
                  -> if ix'' ==# ix || ix'' >=# limit
                        then return (I# ix'')
                        else slurpSome ix''

                 Nothing
                  -> if ix'' ==# ix
                        then return (I# ix'')
                        else slurpSome ix''


         slurp1 ix 
          | Just (I# limit) <- stop
          , ix >=# limit
          =     iwrite refCount 0# ix

          |  otherwise
          =  get1 $ \r
          -> case r of
                Yield1 x switch
                 -> do  
                        write (I# ix) x

                        -- Touch 'x' here beacuse we don't want the code
                        -- that computes it to be floated into the switch
                        -- and then copied.
                        touch x

                        if switch 
                         then iwrite refCount 0# (ix +# 1#)
                         else slurp1 (ix +# 1#)

                Done
                 ->     iwrite refCount 0# ix
                        

         slurp8 ix
          | Just (I# limit)     <- stop
          , ix +# 8# ># limit
          =     iwrite refCount 0# ix

          | otherwise
          =  get8 $ \r
          -> case r of
                Yield8 x0 x1 x2 x3 x4 x5 x6 x7
                 -> do  write (I# (ix +# 0#)) x0
                        write (I# (ix +# 1#)) x1
                        write (I# (ix +# 2#)) x2
                        write (I# (ix +# 3#)) x3
                        write (I# (ix +# 4#)) x4
                        write (I# (ix +# 5#)) x5
                        write (I# (ix +# 6#)) x6
                        write (I# (ix +# 7#)) x7
                        slurp8 (ix +# 8#)

                Pull1   
                 ->     iwrite refCount 0# ix

        slurpSome start
{-# INLINE [0] slurp #-}


class Touch a where
 touch :: a -> IO ()

 here  :: a -> IO a
 here x
  = do  touch x
        return x
 {-# INLINE here #-}

instance Touch Bool where
 touch x
  = IO (\state -> case touch# x state of
                        state' -> (# state', () #))


instance Touch Int where
 touch (I# x)
  = IO (\state -> case touch# x state of
                        state' -> (# state', () #))

instance Touch Double where
 touch (D# x)
  = IO (\state -> case touch# x state of
                        state' -> (# state', () #))

instance (Touch a, Touch b) => Touch (a, b) where
 touch (x, y)
  = do  touch x
        touch y


-- Shorthands -----------------------------------------------------------------
unew    = UM.unsafeNew
uread   = UM.unsafeRead
uwrite  = UM.unsafeWrite


-- | Allocate a vector of ints.
inew :: Int -> IO (UM.IOVector Int)
inew  len = UM.unsafeNew len
{-# INLINE inew #-}


-- | Read an unboxed int from a vector.
iread :: UM.IOVector Int -> Int# -> IO Int
iread  vec ix
 = do   !x      <- UM.unsafeRead vec (I# ix)
        return x
{-# INLINE iread #-}


-- | Write an unboxed into to a vector.
iwrite :: UM.IOVector Int -> Int# -> Int# -> IO ()
iwrite vec ix x 
        = UM.unsafeWrite vec (I# ix) (I# x)
{-# INLINE iwrite #-}

