
module Data.Array.Repa.Flow.Seq.Operator.Pack
        ( packByTag
        , packByFlag
        , filter)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.Operator.Map
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude hiding (map, filter)
import GHC.Exts


-------------------------------------------------------------------------------
-- | Produce only the elements that have their corresponding flag set to `1`.
---  TODO: This can only produce elements one at a time.
--   Use a buffer instead to collect elements from the source.
packByTag :: Unbox a => Flow mode (Int, a) -> Flow mode a
packByTag (Flow start size report get1 get8)
 = Flow start' size' report' get1' get8'
 where
        here    = "seq.packByTag"

        start'
         = do   state   <- start

                -- Buffer to hold elements that we've read from the source.
                buf     <- unew 8

                -- Number of elements in the buffer.
                refLen  <- unew 1
                uwrite here refLen 0 (0 :: Int)

                -- Where we're reading elements from.
                refIx   <- unew 1
                uwrite here refIx 0 (0 :: Int)
                return (state, buf, refLen, refIx)
        {-# INLINE start' #-}

        size' (stateA, _, _, _)
         = do   sz      <- size stateA
                return  $ case sz of
                           Exact len       -> Max len
                           Max   len       -> Max len
        {-# INLINE size' #-}

        report' (stateA, _, _, _)
         = do   r       <- report stateA
                return  $ R.PackByTag r
        {-# NOINLINE report' #-}

        get1' (!stateA, !buf, !refLen, !refIx) push1
         = load
         where  load
                 = do   !(I# ix)  <- uread here refIx  0
                        !(I# len) <- uread here refLen 0
        
                        -- If we already have some elements in the buffer
                        -- then we can use those.
                        if ix <# len
                         then drains ix
                         else fill8
                {-# INLINE load #-}


                -- Fill the buffer eight elements at a time.
                fill8 
                 = get8 stateA $ \xx
                 -> do  !r       <- eat8 xx
                        touch r
                        case r of
                         0              -> fill1

                         _ 
                          -> do !(I# len)   <- uread here refLen 0
                                if len ># 0# 
                                    then drains 0#
                                    else fill8

                eat8 xx
                 =  case xx of
                        Yield8  (b0, x0) (b1, x1) (b2, x2) (b3, x3)
                                (b4, x4) (b5, x5) (b6, x6) (b7, x7)
                         -> do  
                                let ix0  = 0

                                uwrite here buf ix0 x0
                                let !ix1 = ix0 + b0

                                uwrite here buf ix1 x1
                                let !ix2 = ix1 + b1

                                uwrite here buf ix2 x2
                                let !ix3 = ix2 + b2
                                
                                uwrite here buf ix3 x3
                                let !ix4 = ix3 + b3
                                
                                uwrite here buf ix4 x4
                                let !ix5 = ix4 + b4
                                
                                uwrite here buf ix5 x5
                                let !ix6 = ix5 + b5
                                
                                uwrite here buf ix6 x6
                                let !ix7 = ix6 + b6
                                
                                uwrite here buf ix7 x7
                                let !(I# len) = ix7 + b7

                                -- update the buffer length
                                uwrite here refLen 0 (I# len)

                                -- tell fill8 that we might have some elements in the buffer now.
                                return (1 :: Int)

                        Pull1 
                         -> do  -- tell fill8 that we need to pull one at a time.
                                return (0 :: Int)
                {-# INLINE eat8 #-}


                -- Fill the buffer one element at a time.
                fill1
                 = get1 stateA $ \xx
                 -> do  r       <- eat1 xx
                        touch r
                        case r of
                         0              -> push1 Done
                         2              -> drains 0#
                         _              -> fill1

                eat1 xx
                 = case xx of
                        Yield1 (0, _) _ 
                         ->     return 3

                        Yield1 (_, x) _ 
                         -> do  uwrite here buf 0 x
                                return 2

                        Done
                         -> return (0 :: Int)
                {-# INLINE eat1 #-}

                drains ix
                 = do   -- Update the index
                        let !ix' = ix +# 1#
                        uwrite here refIx 0 (I# ix')

                        -- Push the element to the consumer.
                        !x      <- uread here buf (I# ix)
                        push1 $ Yield1  x False
                {-# INLINE drains #-}

        {-# INLINE get1' #-}


         -- We can't deliver 8 elements at a time because there might not
         -- be that many in the source flow that match the predicate.
         --  TODO: maybe the get8 function should take both push8 and push1
        get8' _ push8
         = push8 $ Pull1
        {-# INLINE get8' #-}

{-# INLINE [1] packByTag #-}


-------------------------------------------------------------------------------
-- | Produce only those elements that have their corresponding
--   flag set to `True`
packByFlag :: Unbox a => Flow mode (Bool, a) -> Flow mode a
packByFlag ff
        = packByTag $ map (\(b, x) -> (I# (tagOfFlag b), x)) ff
{-# INLINE [1] packByFlag #-}


tagOfFlag :: Bool -> Int#
tagOfFlag b
 = if b then 1# else 0#
{-# NOINLINE tagOfFlag #-}

-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: Unbox a => (a -> Bool) -> Flow mode a -> Flow mode a
filter f ff
        = packByFlag $ map (\x -> (f x, x)) ff
{-# INLINE [1] filter #-}

