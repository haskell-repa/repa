
module Data.Array.Repa.Flow.Seq.Append
        (appends)
where
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as Report
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
        here            = "repa-flow.appends"

        sTakeFrom       = 0#
        sSegOff         = 1#
        sIndexA         = 2#
        sIndexB         = 3#
        sNextSwap       = 4#
        sRemain         = 5#

        start
         -- The result vector has no elements, so we're already done.
         | n ==# 0#
         = do   state <- unew 6
                iwrite here state sTakeFrom   0#
                iwrite here state sSegOff     0#
                iwrite here state sIndexA     0#
                iwrite here state sIndexB     0#
                iwrite here state sNextSwap   0#
                iwrite here state sRemain     0#
                return state

         -- Start reading elements from the A vector.
         | el_off <# segLenA seg_off
         = do   state   <- unew 6
                iwrite here state sTakeFrom  0#
                iwrite here state sSegOff    seg_off
                iwrite here state sIndexA    (segIdxA seg_off +# el_off)
                iwrite here state sIndexB    (segIdxB seg_off)
                iwrite here state sNextSwap  (segLenA seg_off -# el_off)
                iwrite here state sRemain    n
                return state

        -- Start reading elements from the B vector.
         | otherwise
         = do   state   <- unew 6
                -- Get the starting element offset relative to the first segment 
                -- of the Y array.
                let !el_off'  = el_off -# segLenA seg_off
                iwrite here state sTakeFrom  1#
                iwrite here state sSegOff    seg_off
                iwrite here state sIndexA    (segIdxA seg_off +# segLenA seg_off)
                iwrite here state sIndexB    (segIdxB seg_off +# el_off')
                iwrite here state sNextSwap  (segLenB seg_off -# el_off')
                iwrite here state sRemain    n
                return state

        size state
         = do   !(I# remain)  <- iread here state sRemain 
                return $ Exact remain


        -- TODO: report incremental state
        report _state
         = return
                $ Report.Appends
                { Report.appendsResultLen      = (I# n) }


        get1 state push1
         = do   !(I# remain)    <- iread here state sRemain

                if remain <=# 0#
                 then push1 Done
                 else do
                        !(I# takeFrom)  <- iread here state sTakeFrom
                        if takeFrom ==# 0# 
                         then get1_fromA state push1
                         else get1_fromB state push1

        get1_fromA state push1
         = do   !(I# nextSwap)  <- iread here state sNextSwap

                if nextSwap ==# 0#
                 -- Done reading the A vector for now, so switch to B       
                 then do
                        !(I# segOff)    <- iread here state sSegOff
                        iwrite here state sTakeFrom 1#
                        iwrite here state sNextSwap (segLenB segOff)
                        get1_fromB state push1

                 -- Emit a value from the A vector.
                 else do
                        !(I# indexA)    <- iread here state sIndexA
                        !(I# remain)    <- iread here state sRemain
                        iwrite here state sIndexA   (indexA   +# 1#)
                        iwrite here state sNextSwap (nextSwap -# 1#)
                        iwrite here state sRemain   (remain   -# 1#)
                        send1 push1 (elemA indexA)

        get1_fromB state push1
         = do   !(I# nextSwap)  <- iread here state sNextSwap

                if nextSwap ==# 0#
                 -- Done reading the B vector for now, so switch to A
                 then do
                        !(I# segOff)    <- iread here state sSegOff
                        let !segOff'    = segOff +# 1#
                        iwrite here state sTakeFrom 0#
                        iwrite here state sNextSwap (segLenA segOff')
                        iwrite here state sSegOff   segOff'
                        get1_fromA state push1

                 -- Emit a value from the B vector.
                 else do
                        !(I# indexB)    <- iread here state sIndexB
                        !(I# remain)    <- iread here state sRemain
                        iwrite here state sIndexB   (indexB   +# 1#)
                        iwrite here state sNextSwap (nextSwap -# 1#)
                        iwrite here state sRemain   (remain   -# 1#)
                        send1 push1 (elemB indexB)

        send1 push1 x
         = push1 (Yield1 x False)

        -- TODO: It should be straight forward to provide 8 elements at a time
        --       if there are at least that many remaining in the current segment.
        get8 _ push8
         = push8 $ Pull1

