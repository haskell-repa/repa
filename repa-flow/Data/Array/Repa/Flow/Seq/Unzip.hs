
module Data.Array.Repa.Flow.Seq.Unzip
        (unzip)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude hiding (unzip)
import Data.IORef
import GHC.Exts


-- | Unzip a flow of tuples into flows of the components.
--   TODO: generalise this into the 'dup' function instead.
unzip   :: (Unbox a, Unbox b) 
        => Flow mode (a, b) -> IO (Flow mode a, Flow mode b)

unzip (Flow startIn sizeIn reportIn get1 _get8)
 = do   
        let here    = "seq.unzip"

        -- Current state of unzip buffer.
        --  0 - no element in buffer
        --  1 - there is an element in the left buffer,  waiting to be pulled.
        --  2 - there is an element in the right buffer, waiting to be pulled.
        let sMode   = 0#

        refStateIn      <- newIORef Nothing

        state           <- inew 1
        iwrite here state sMode 0#

        bufLeft         <- unew 1
        bufRight        <- unew 1

        let 
         -- We only need to initialise the state of the in-flow once, 
         -- not once per out-flow.
         {-# NOINLINE start' #-}
         start' 
          = do  mStateIn        <- readIORef refStateIn
                case mStateIn of
                 Nothing        
                  -> do stateIn <- startIn
                        writeIORef refStateIn (Just stateIn)
                        return stateIn

                 Just stateIn
                  ->    return stateIn


         -- Get function for left flow.
         getLeft1 stateIn push1
          = do   !(I# mode)      <- iread here state sMode
                 next mode

          where next mode
                 = case mode of
                    -- No elem in buffer, pull a new one from source.
                    0# -> get1 stateIn $ \r
                       -> case r of
                           Yield1 (x, y) _    
                            -> do uwrite here bufRight 0     y
                                  iwrite here state    sMode 2#
                                  emitYield1 x

                           Stall -> emitStall
                           Done  -> push1 Done

                    -- Use element from the left buffer.
                    1# -> do     
                         x       <- uread here bufLeft 0 
                         iwrite here state sMode 0#
                         emitYield1 x

                    -- We're still stalled waiting for the right flow.
                    _  -> emitStall

                emitYield1 x
                 = push1 $ Yield1 x False

                emitStall
                 = push1 $ Stall

         getLeft8 _ push8
          = push8 Pull1


         -- Get function for right flow.
         getRight1 stateIn push1
          = do   !(I# mode)      <- iread here state sMode
                 next mode

          where next mode
                  = case mode of
                    -- No elem in buffer, pull a new one from source.
                    0# -> get1 stateIn $ \r
                       -> case r of
                           Yield1 (x, y) _    
                            -> do uwrite here bufLeft 0     x
                                  iwrite here state   sMode 1#
                                  emitYield1 y

                           Stall -> emitStall
                           Done  -> push1 Done

                    -- Use element from the right buffer.
                    2# -> do     
                        y       <- uread here bufRight 0
                        iwrite here state sMode 0#
                        emitYield1 y

                    -- We're still stalled waiting for the left flow.
                    _  -> emitStall

                emitYield1 x
                 = push1 $ Yield1 x False

                emitStall
                 = push1 $ Stall

         getRight8 _ push8 
          = push8 Pull1

        return ( Flow start' sizeIn reportIn getLeft1  getLeft8
               , Flow start' sizeIn reportIn getRight1 getRight8)
{-# INLINE [4] unzip #-}
