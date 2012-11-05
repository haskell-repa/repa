
module Data.Array.Repa.Flow.Base
        ( Flow(..), Step1(..), Step8(..)
        , Size(..)
        , sizeMin
        , flow
        , unflow
        , take
        , Touch(..))
where
import GHC.Exts
import GHC.Types
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import System.IO.Unsafe
import Prelude                                  hiding (take)


-- | Flows provide an incremental version of array fusion that allows the
--   the computation to be suspended and resumed at a later time.
-- 
--   Using the `flowGet8` interface, eight elements of a flow are computed for
--   each loop interation, producing efficient object code.
data Flow a
        = forall state. Flow
        { 
          -- | Start the flow. 
          --   This returns a state value that needs to be passed to
          --   the get functions.
          flowStart     :: IO state

          -- | How many elements are available in this flow.
        , flowSize      :: state -> IO Size

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
        --   We don't want to *force* the consumer to pull the whole 8
        --   if it doesn't want to, otherwise functions like folds would
        --   become too complicated.
        = Yield1 a Bool

        -- | Stream is finished.
        | Done


data Step8 a
        -- | Eight successive elements of the flow.
        = Yield8 a a a a a a a a

        -- | Indicates that this flow isn't prepared to yield a full eight
        --   elements right now. You should call `get1` to get the next
        --   element and try `get8` again later.
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
-- | Convert an unboxed vector to a flow.
flow :: (Touch a, U.Unbox a) => U.Vector a -> Flow a
flow !vec
 = Flow start size get1 get8
 where  
        start
         = do   refIx   <- UM.unsafeNew 1
                UM.unsafeWrite refIx 0 0
                return refIx
        {-# INLINE start #-}


        size refIx
         = do   let !(I# len)    = U.length vec
                !(I# ix) <- UM.unsafeRead refIx 0
                return  $ Exact (len -# ix)
        {-# INLINE size #-}


        get1 refIx push1
         = do   let !(I# len)    = U.length vec
                !(I# ix)        <- UM.unsafeRead refIx 0
                let !remain     =  len -# ix
                if remain ># 0#
                 then do
                        UM.unsafeWrite refIx 0 (I# (ix +# 1#))
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
         = do   let !(I# len)    = U.length vec
                !(I# ix) <- UM.unsafeRead refIx 0
                let !remain     = len -# ix
                if remain >=# 8#
                 then do
                        UM.unsafeWrite refIx 0 (I# (ix +# 8#))

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
-- | Fully evaluate a flow, producing an unboxed vector.
-- 
--   TODO: when this is applied to a stateful flow it should really be 
--   an IO action, because the argument can't be unflowed again.
--
--   Add a type index to say whether the flow has already started, 
--   and an IO function 'drain' that finishes it.
--
unflow :: (Touch a, U.Unbox a) => Flow a -> U.Vector a
unflow !ff
 = unsafePerformIO 
 $ case ff of
    Flow fStart fSize fGet1 fGet8
     -> do !state   <- fStart
           !size    <- fSize state 
           case size of
            Exact len       -> unflowExact len (fGet1 state) (fGet8 state)
            Max   len       -> unflowExact len (fGet1 state) (fGet8 state)
                                        -- TODO: this is wrong
{-# INLINE [1] unflow #-}


unflowExact 
        :: (Touch a, U.Unbox a) 
        => Int# 
        -> ((Step1 a -> IO ()) -> IO ())
        -> ((Step8 a -> IO ()) -> IO ())
        -> IO (U.Vector a)

unflowExact !len get1 get8
 = do   !mvec    <- UM.unsafeNew (I# len)
        !len'    <- slurp Nothing (UM.unsafeWrite mvec) get1 get8
        !vec     <- U.unsafeFreeze mvec
        return   $  U.unsafeSlice 0 len' vec
{-# INLINE [1] unflowExact #-}


-------------------------------------------------------------------------------
-- | Take the given number of elements from the front of a flow,
--   returning a vector of elements and the rest of the flow.
--   
--   The returned flow is stateful and will only provide its elements once.
--
take    :: (Touch a, U.Unbox a) 
        => Int# -> Flow a -> IO (U.Vector a, Flow a)

take limit (Flow start size get1 get8)
 = do   state    <- start

        !mvec    <- UM.unsafeNew (I# limit)
        !len'    <- slurp (Just (I# limit)) (UM.unsafeWrite mvec) 
                        (get1 state) (get8 state)
        !vec     <- U.unsafeFreeze mvec
        let !vec' = U.unsafeSlice 0 len' vec        
        return (vec', Flow (return state) size get1 get8)
{-# INLINE [1] take #-}


-------------------------------------------------------------------------------
-- | Slurp out all the elements from a flow,
--   passing them to the provided consumption function.
slurp   :: Touch a
        => Maybe Int
        -> (Int -> a -> IO ())
        -> ((Step1 a  -> IO ()) -> IO ())
        -> ((Step8 a  -> IO ()) -> IO ())
        -> IO Int

slurp stop !write get1 get8
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 (-1)

        let
         slurpSome ix
          = do  slurp8 ix
                I# ix'     <- UM.unsafeRead refCount 0 

                slurp1 ix' 
                I# ix''    <- UM.unsafeRead refCount 0

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
          =     UM.unsafeWrite refCount 0 (I# ix)

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
                         then UM.unsafeWrite refCount 0 (I# (ix +# 1#))
                         else slurp1 (ix +# 1#)

                Done
                 ->     UM.unsafeWrite refCount 0 (I# ix)
                        

         slurp8 ix
          | Just (I# limit)     <- stop
          , ix +# 8# ># limit
          =     UM.unsafeWrite refCount 0 (I# ix)

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
                 ->     UM.unsafeWrite refCount 0 (I# ix)

        slurpSome 0#
{-# INLINE [0] slurp #-}

class Touch a where
 touch :: a -> IO ()

 here  :: a -> IO a
 here x
  = do  touch x
        return x
 {-# INLINE here #-}


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


