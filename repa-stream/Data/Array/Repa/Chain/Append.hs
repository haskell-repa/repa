
module Data.Array.Repa.Chain.Append
        (appendSegs)
where
import Data.Array.Repa.Chain.Base
import GHC.Exts


-- | Segmented append.
appendSegs
        :: forall a 
        .  (Int# -> Int#)   -- ^ Get segment lengths for 'xs' array.
        -> (Int# -> Int#)   -- ^ Get segment indices for 'xs' array.
        -> (Int# -> a)      -- ^ Get data from the 'xs' array.

        -> (Int# -> Int#)   -- ^ Get segment lengths for 'ys' array.
        -> (Int# -> Int#)   -- ^ Get segment indices for 'ys' array.
        -> (Int# -> a)      -- ^ Get data from the 'ys' array.

        -> Int#             -- ^ How many result elements to produce in total.
        -> Int#             -- ^ Starting segment id.
        -> Int#             -- ^ Starting element index within the starting segment.
                            --   This is the element index relative to the result array.
        -> Chain a

appendSegs segLenX segIdxX elemX 
           segLenY segIdxY elemY
           n seg_off el_off
 = Chain n state next
 where
        -- Starting State -------------
        -- We need to determine which vector to start reading elements from.
        state
         -- The result vector has no elements, so we're already done.
         | n ==# 0# 
         = ASDo { as_takefrom   = 0#
                , as_seg_off    = 0#
                , as_xs_index   = 0#
                , as_ys_index   = 0#
                , as_next_swap  = 0#
                , as_remain     = 0# }

         -- Start reading elements from xs.
         --  If the starting element index is less than the length of the
         --  first 'xs' segment then we start reading elements from the
         --  'xs' array.
         | el_off <# segLenX seg_off 
         = ASDo { as_takefrom   = 0#
                , as_seg_off    = seg_off
                , as_xs_index   = segIdxX seg_off +# el_off
                , as_ys_index   = segIdxY seg_off 
                , as_next_swap  = segLenX seg_off -# el_off
                , as_remain     = n }

         -- Start reading elements from ys.
         --  We still set as_xs_index to the first element we will read from
         --  the X array, but in this case it will be for the next segment.
         --
         --  NOTE: *not* indicesUSegd xd ! (seg_off+1) since seg_off+1
         --        might be out of bounds   
         | otherwise
         = let -- Get the starting element offset relative to the first segment 
               -- of the Y array.
               !el_off'  = el_off -# segLenX seg_off
           in  ASDo
                { as_takefrom   = 1#
                , as_seg_off    = seg_off
                , as_xs_index   = segIdxX seg_off +# segLenX seg_off     
                , as_ys_index   = segIdxY seg_off +# el_off'
                , as_next_swap  = segLenY seg_off -# el_off'
                , as_remain     = n }
        {-# INLINE next #-}

        -- Next -----------------------
        next :: Int# -> AppendState -> Step AppendState a
        -- Reading from xs
        next _ s@ASDo { as_takefrom = 0# }
         -- Done reading xs, so read the rest of this segment from ys.
         | as_next_swap s  ==# 0#
         = Update $ s   { as_takefrom      = 1#
                        , as_next_swap     = segLenY (as_seg_off s) }

         -- Emit a value from xs.
         | otherwise  
         = Yield (inc s) (elemX (as_xs_index s))


        -- Reading from ys.
        next _ s
         -- Done reading ys, so read the rest from the next segments xs.
         | as_next_swap s  ==# 0#
         = let seg'     = as_seg_off s +# 1#
           in  Update 
                $ s     { as_takefrom   = 0#
                        , as_seg_off    = seg'
                        , as_next_swap  = segLenX seg' }

         -- Emit a value from ys
         | otherwise 
         = Yield (inc s) (elemY (as_ys_index s))


        -- Inc ---------------------------
        -- Move data pointer forward, and decrease remaining and swap
        inc :: AppendState -> AppendState
        inc s@ASDo 
                { as_takefrom   = 0#
                , as_xs_index   = xi
                , as_next_swap  = swap
                , as_remain     = n'}

         = s    { as_xs_index   = xi   +# 1#
                , as_next_swap  = swap -# 1#
                , as_remain     = n'   -# 1# }

        -- Takefrom is nonzero: reading from ys
        inc s@ASDo
                { as_ys_index   = yi
                , as_next_swap  = swap
                , as_remain     = n'}

         = s    { as_ys_index   = yi   +# 1#
                , as_next_swap  = swap -# 1#
                , as_remain     = n'   -# 1# }
        {-# INLINE inc #-}

{-# INLINE appendSegs #-}


data AppendState
        = ASDo
        { -- | Which vector we're currently reading from.
          --   0 = xs, nonzero = ys
          as_takefrom  :: Int#   

          -- | Current segment id.
        , as_seg_off   :: Int#

          -- | Current index into 'xs' data.
        , as_xs_index  :: Int#

          -- | Current index into 'ys' data.
        , as_ys_index  :: Int#
 
          -- | How many elements to emit before we swap source arrays.
        , as_next_swap :: Int#   

          -- | How many more elements we need to emit in total.
        , as_remain    :: Int#
        }
