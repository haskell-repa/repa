
module Data.Array.Repa.Flow.Seq.Buildable
        ( Builder (..)
        , buildSumInt

        , build1
        , build2

        , slurpInto)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Seq.Base
import Data.Vector.Unboxed              (Vector, Unbox)
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import GHC.Exts


data Builder a b
        = forall state. Builder
        { buildInit     :: Size  -> IO state
        , buildAcc1     :: state -> a   -> IO ()
        , buildFreeze   :: state        -> IO b }


-- | Builder to reduce a flow into an int via summation.
buildSumInt :: Builder Int Int
buildSumInt
        = Builder
        { buildInit     = \len ->
           do   ref     <- UM.unsafeNew 1
                UM.unsafeWrite ref 0 (0 :: Int)
                return ref

        , buildAcc1     = \ref x' ->
           do   x       <- UM.unsafeRead ref 0
                UM.unsafeWrite ref 0 (x + x')

        , buildFreeze   = \ref ->
           do   UM.unsafeRead  ref 0 }
{-# INLINE [1] buildSumInt #-}


-- Build1 ---------------------------------------------------------------------
-- | Build a result from a single flow.
build1  :: Elt a 
        => Builder a b -> Flow mode a
        -> IO b

build1  (Builder bInit  bAcc1     bFreeze)
        (Flow    fStart fGetSize  _fGetReport fGet1 fGet8)
 = do   
        -- Start the flow and get the expected flow size.
        fState  <- fStart
        fSize   <- fGetSize fState

        -- Initialise the builder.
        bState  <- bInit fSize

        let eat1
             = do (I# count, I# complete) 
                   <- slurpInto   (bAcc1 bState) (fGet1 fState) (fGet8 fState)

                  if complete ==# 1#
                   then return ()
                   else eat1

        eat1
        bFreeze bState
{-# INLINE [1] build1 #-}


-- Build2 ---------------------------------------------------------------------
-- Build two results from two linked flows.
build2  :: (Elt a1, Elt a2)
        => Builder a1 b1 -> Flow mode1 a1 
        -> Builder a2 b2 -> Flow mode2 a2 
        -> IO (b1, b2)

build2  (Builder b1Init   b1Acc1       b1Freeze)
        (Flow    f1Start  f1GetSize _  f1Get1 f1Get8)
        (Builder b2Init   b2Acc1       b2Freeze)
        (Flow    f2Start  f2GetSize _  f2Get1 f2Get8)
 = do   
        -- Start the flows, and initialise the builders.
        f1State <- f1Start;  f1Size <- f1GetSize f1State;  b1State <- b1Init f1Size
        f2State <- f2Start;  f2Size <- f2GetSize f2State;  b2State <- b2Init f2Size

        let 
         eat2
          = do  (I# count1, I# complete1)
                 <- slurpInto (b1Acc1 b1State) (f1Get1 f1State) (f1Get8 f1State)

                (I# count2, I# complete2)
                 <- slurpInto (b2Acc1 b2State) (f2Get1 f2State) (f2Get8 f2State)

                putStrLn "eat2 loop"

                if   complete1 ==# 1# 
                  && complete2 ==# 1#
                  then return ()
                  else eat2

        eat2
        result1 <- b1Freeze b1State
        result2 <- b2Freeze b2State
        return  (result1, result2)
{-# INLINE [1] build2 #-}


-- Slurp ----------------------------------------------------------------------
-- | Slurp elements from a flow and feed them into a builder.
slurpInto 
        :: Elt a
        => (a      -> IO ())
        -> ((Step1 a -> IO ()) -> IO ())
        -> ((Step8 a -> IO ()) -> IO ())
        -> IO (Int, Int)

slurpInto bAcc1 fGet1 fGet8
 = do   let here        = "flow.seq.slurpInto"

        -- Slurp state.
        let sCount      = 0#    -- Total number of elements slurped.
        let sComplete   = 1#    -- Whether we've completed the whole flow.
        state           <- inew 2

        iwrite here state 0# 0#
        iwrite here state 1# 0#

        let slurp count
             = do       slurp1 count
                        I# count'  <- iread here state sCount

                        if count ==# count'
                         then return (I# count')
                         else slurp  count'

            -- Try to slurp a single element from the flow.
            slurp1 count
             =  fGet1 $ \r
             -> case r of
                 -- We got an element from the flow.
                 Yield1 x switch
                  -> do 
                        -- Accumulate the element into the result builder.
                        bAcc1 x

                        -- Touch 'x' here because we don't want the code
                        -- that computes it to be floated into the switch 
                        -- and then copied.
                        touch x

                        if switch 
                         then do
                                iwrite here state sCount    (count +# 1#)
                                iwrite here state sComplete 0#

                         else   slurp1 (count +# 1#)

                 -- The flow doesn't have any elements for us right now.
                 -- Remember the current element count and the fact that 
                 --  we haven't completed draining the flow.
                 Stall  
                  -> do iwrite here state sCount    count
                        iwrite here state sComplete 0#

                 -- The flow is done, all elements have already been read.
                 Done   
                  -> do iwrite here state sCount    count
                        iwrite here state sComplete 1#

        I# count'   <- slurp 0#
        I# complete <- iread here state sComplete
        return   (I# count', I# complete)
{-# INLINE [0] slurpInto #-}

