
module Data.Array.Repa.Flow.Seq.Append
        (appends)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as Report
import qualified Data.Vector.Unboxed.Mutable            as UM
import GHC.Exts


-- | Segmented append.
appends
        :: (Int# -> Int#)   -- ^ Get segment lengths for 'xs' array.
        -> (Int# -> Int#)   -- ^ Get segment indices for 'xs' array.
        -> (Int# -> a)      -- ^ Get data from the 'xs' array.

        -> (Int# -> Int#)   -- ^ Get segment lengths for 'ys' array.
        -> (Int# -> Int#)   -- ^ Get segment indices for 'ys' array.
        -> (Int# -> a)      -- ^ Get data from the 'ys' array.

        -> Int#             -- ^ How many result elements to produce in total.
        -> Int#             -- ^ Starting segment id.
        -> Int#             -- ^ Starting element index within the starting segment.
                            --   This is the element index relative to the result array.
        -> Flow mode a

appends segLenA segIdxA elemA
        segLenB segIdxB elemB
        n seg_off el_off
 = Flow start size report get1 get8
 where
        sTakeFrom       = 0
        sSegOff         = 1
        sIndexA         = 2
        sIndexB         = 3
        sNextSwap       = 4
        sRemain         = 5

        start
         -- The result vector has no elements, so we're already done.
         | n ==# 0#
         = do   state   <- UM.unsafeNew 6
                UM.unsafeWrite state sTakeFrom (0 :: Int)
                UM.unsafeWrite state sSegOff   (0 :: Int)
                UM.unsafeWrite state sIndexA   (0 :: Int)
                UM.unsafeWrite state sIndexB   (0 :: Int)
                UM.unsafeWrite state sNextSwap (0 :: Int)
                UM.unsafeWrite state sRemain   (0 :: Int)
                return state

         -- Start reading elements from the A vector.
         | el_off <# segLenA seg_off
         = do   state   <- UM.unsafeNew 6
                UM.unsafeWrite state sTakeFrom  (0 :: Int)
                UM.unsafeWrite state sSegOff    (I# seg_off)
                UM.unsafeWrite state sIndexA    (I# (segIdxA seg_off +# el_off))
                UM.unsafeWrite state sIndexB    (I# (segIdxB seg_off))
                UM.unsafeWrite state sNextSwap  (I# (segLenA seg_off -# el_off))
                UM.unsafeWrite state sRemain    (I# n)
                return state

        -- Start reading elements from the B vector.
         | otherwise
         = do   state   <- UM.unsafeNew 6
                -- Get the starting element offset relative to the first segment 
                -- of the Y array.
                let !el_off'  = el_off -# segLenA seg_off
                UM.unsafeWrite state sTakeFrom  (1 :: Int)
                UM.unsafeWrite state sSegOff    (I# seg_off)
                UM.unsafeWrite state sIndexA    (I# (segIdxA seg_off +# segLenA seg_off))
                UM.unsafeWrite state sIndexB    (I# (segIdxB seg_off +# el_off'))
                UM.unsafeWrite state sNextSwap  (I# (segLenB seg_off -# el_off'))
                UM.unsafeWrite state sRemain    (I# n)
                return state


        size state
         = do   !(I# remain)  <- UM.unsafeRead state sRemain 
                return $ Exact remain


        -- TODO: report incremental state
        report _state
         = return
                $ Report.Appends
                { Report.appendsResultLen      = (I# n) }


        get1 state push1
         = do   !(I# remain)    <- UM.unsafeRead state sRemain

                if remain <=# 0#
                 then push1 Done
                 else do
                        !(I# takeFrom)  <- UM.unsafeRead state sTakeFrom
                        if takeFrom ==# 0# 
                         then get1_fromA state push1
                         else get1_fromB state push1

        get1_fromA state push1
         = do   !(I# nextSwap)  <- UM.unsafeRead state sNextSwap

                if nextSwap ==# 0#
                 -- Done reading the A vector for now, so switch to B       
                 then do
                        !(I# segOff)    <- UM.unsafeRead state sSegOff
                        UM.unsafeWrite state sTakeFrom 1
                        UM.unsafeWrite state sNextSwap (I# (segLenB segOff))
                        get1_fromB state push1

                 -- Emit a value from the A vector.
                 else do
                        !(I# indexA)    <- UM.unsafeRead state sIndexA
                        !(I# remain)    <- UM.unsafeRead state sRemain
                        UM.unsafeWrite state sIndexA   (I# (indexA   +# 1#))
                        UM.unsafeWrite state sNextSwap (I# (nextSwap -# 1#))
                        UM.unsafeWrite state sRemain   (I# (remain   -# 1#))
                        send1 push1 (elemA indexA)

        get1_fromB state push1
         = do   !(I# nextSwap)  <- UM.unsafeRead state sNextSwap

                if nextSwap ==# 0#
                 -- Done reading the B vector for now, so switch to A
                 then do
                        !(I# segOff)    <- UM.unsafeRead state sSegOff
                        let !segOff'    = segOff +# 1#
                        UM.unsafeWrite state sTakeFrom 0
                        UM.unsafeWrite state sNextSwap (I# (segLenB segOff'))
                        UM.unsafeWrite state sSegOff   (I# segOff')
                        get1_fromA state push1

                 -- Emit a value from the B vector.
                 else do
                        !(I# indexB)    <- UM.unsafeRead state sIndexB
                        !(I# remain)    <- UM.unsafeRead state sRemain
                        UM.unsafeWrite state sIndexB   (I# (indexB   +# 1#))
                        UM.unsafeWrite state sNextSwap (I# (nextSwap -# 1#))
                        UM.unsafeWrite state sRemain   (I# (remain   -# 1#))
                        send1 push1 (elemB indexB)

        send1 push1 x
         = push1 (Yield1 x False)


        -- TODO: It should be straight forward to provide 8 elements at a time
        --       if there are at least that many remaining in the current segment.
        get8 _ push8
         = push8 $ Pull1

