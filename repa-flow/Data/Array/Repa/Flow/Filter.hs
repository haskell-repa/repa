module Data.Array.Repa.Flow.Filter
        ( packByTag
        , pack
        , filter)
where
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Map
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude hiding (map, filter)
import GHC.Exts


-------------------------------------------------------------------------------
-- | Produce only the elements that have their corresponding flag set to `True`.
---  TODO: This can only produce elements one at a time.
--   Use a buffer instead to collect elements from the source.
packByTag :: U.Unbox a => Flow (Int, a) -> IO (Flow a)
packByTag (Flow getSize get1 get8)
 = do   
        -- Buffer to hold elements that we've read from the source.
        buf     <- UM.unsafeNew 8

        -- Number of elements in the buffer.
        refLen  <- UM.unsafeNew 1
        UM.unsafeWrite refLen 0 (0 :: Int)

        -- Where we're reading elements from.
        refIx   <- UM.unsafeNew 1
        UM.unsafeWrite refIx 0 (0 :: Int)


        let 
         getSize'
          = do  size    <- getSize
                return  $ case size of
                           Exact len       -> Max len
                           Max   len       -> Max len

         get1' push1
          = load
          where
                load
                 = do   !(I# ix)  <- UM.unsafeRead refIx  0
                        !(I# len) <- UM.unsafeRead refLen 0
        
                        -- If we already have some elements in the buffer
                        -- then we can use those.
                        if ix <# len
                         then drain ix
                         else fill8

                drain ix
                 = do   -- Update the index
                        let !ix' = ix +# 1#
                        UM.unsafeWrite refIx 0 (I# ix')

                        -- Push the element to the consumer.
                        !x      <- UM.unsafeRead buf (I# ix)
                        push1 $ Yield1  x False

                fill8
                 =  get8 $ \r
                 -> case r of
                        Yield8  (b0, x0) (b1, x1) (b2, x2) (b3, x3)
                                (b4, x4) (b5, x5) (b6, x6) (b7, x7)
                         -> do  
                                let ix0  = 0

                                UM.unsafeWrite buf ix0 x0
                                let !ix1 = ix0 + b0

                                UM.unsafeWrite buf ix1 x1
                                let !ix2 = ix1 + b1

                                UM.unsafeWrite buf ix2 x2
                                let !ix3 = ix2 + b2
                                
                                UM.unsafeWrite buf ix3 x3
                                let !ix4 = ix3 + b3
                                
                                UM.unsafeWrite buf ix4 x4
                                let !ix5 = ix4 + b4
                                
                                UM.unsafeWrite buf ix5 x5
                                let !ix6 = ix5 + b5
                                
                                UM.unsafeWrite buf ix6 x6
                                let !ix7 = ix6 + b6
                                
                                UM.unsafeWrite buf ix7 x7
                                let !(I# len) = ix7 + b7

                                if len ># 0#
                                 then do
                                        UM.unsafeWrite refLen 0 (I# len)
                                        drain 0#

                                 else fill8

                        Pull1 -> pull1

                pull1
                 =  get1 $ \r
                 -> case r of
                        Yield1 (0, _) _ -> pull1
                        Yield1 (1, x) _ -> push1 (Yield1 x False)
                        Yield1 (_, _) _ -> error "Data.Array.Repa.Flow.Filter: tag out of range"
                        Done            -> push1 Done

         -- We can't deliver 8 elements at a time because there might not
         -- be that many in the source flow that match the predicate.
         --  TODO: maybe the get8 function should take both push8 and push1
         get8' push8
          = push8 $ Pull1

        return $ Flow getSize' get1' get8'

{-# INLINE [1] packByTag #-}

-------------------------------------------------------------------------------
-- | Produce only those elements that have their corresponding flag set.
pack :: U.Unbox a => Flow (Bool, a) -> IO (Flow a)
pack ff
        = packByTag $ map (\(b, x) -> (if b then 1 else 0, x)) ff
{-# INLINE [1] pack #-}

-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: U.Unbox a => (a -> Bool) -> Flow a -> IO (Flow a)
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}
