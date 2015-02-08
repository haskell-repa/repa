
module Data.Repa.Eval.Generic.Seq.Reduction
        ( foldAll
        , foldRange
        , foldInner)
where
import GHC.Exts


-- | Sequential reduction of all the elements in an array.
foldAll :: (Int# -> a)         -- ^ Function to get an element from the source.
        -> (a -> a -> a)       -- ^ Binary associative combining function.
        -> a                   -- ^ Neutral starting value.
        -> Int#                -- ^ Number of elements.
        -> a

foldAll get c !r !len
 = foldRange get c r 0# len 
{-# INLINE [1] foldAll #-}


-- | Sequential reduction of a multidimensional array along the innermost dimension.
foldInner   
        :: (Int# -> a -> IO ()) -- ^ Function to write into the result buffer.
        -> (Int# -> a)          -- ^ Function to get an element from the source.
        -> (a -> a -> a)        -- ^ Binary associative combination function.
        -> a                    -- ^ Neutral starting value.
        -> Int#                 -- ^ Total length of source.
        -> Int#                 -- ^ Inner dimension (length to fold over).
        -> IO ()

foldInner write get c !r !end !n
 = iter 0# 0#
 where
        iter !sh !sz 
         | 1# <- sh >=# end 
         = return ()

         | otherwise 
         = do   let !next = sz +# n
                write sh (foldRange get c r sz next)
                iter (sh +# 1#) next
        {-# INLINE iter #-}
{-# INLINE [1] foldInner #-}


-- Reduce ---------------------------------------------------------------------
-- | Sequentially reduce values between the given indices.
---
--   We use manual specialisations and rewrite rules to avoid the result
--   being boxed up in the final iteration.
foldRange
        :: (Int# -> a)          -- ^ Function to get an element from the source.
        -> (a -> a -> a)        -- ^ Binary associative combining function.
        -> a                    -- ^ Neutral starting value.
        -> Int#                 -- ^ Starting index.
        -> Int#                 -- ^ Ending index.
        -> a

foldRange f c !r !start !end 
 = iter start r
 where  iter !i !z 
         | 1# <- i >=# end  = z 
         | otherwise        = iter (i +# 1#) (f i `c` z)
        {-# INLINE iter #-}
{-# INLINE [0] foldRange #-}


foldRangeInt
        :: (Int# -> Int#)
        -> (Int# -> Int# -> Int#)
        -> Int# 
        -> Int# -> Int# 
        -> Int#

foldRangeInt f c !r !start !end 
 = iter start r
 where  iter !i !z 
         | 1# <- i >=# end  = z 
         | otherwise        = iter (i +# 1#) (f i `c` z)
        {-# INLINE iter #-}
{-# INLINE [0] foldRangeInt #-}


foldRangeFloat
        :: (Int# -> Float#) 
        -> (Float# -> Float# -> Float#)
        -> Float# 
        -> Int# -> Int# 
        -> Float#

foldRangeFloat f c !r !start !end 
 = iter start r
 where  iter !i !z 
         | 1# <- i >=# end  = z 
         | otherwise         = iter (i +# 1#) (f i `c` z)
        {-# INLINE iter #-}
{-# INLINE [0] foldRangeFloat #-}


foldRangeDouble
        :: (Int# -> Double#) 
        -> (Double# -> Double# -> Double#)
        -> Double# 
        -> Int# -> Int# 
        -> Double#

foldRangeDouble f c !r !start !end 
 = iter start r
 where  iter !i !z 
         | 1# <- i >=# end  = z 
         | otherwise        = iter (i +# 1#) (f i `c` z)
        {-# INLINE iter #-}
{-# INLINE [0] foldRangeDouble #-}


unboxInt :: Int -> Int#
unboxInt (I# i) = i
{-# INLINE unboxInt #-}


unboxFloat :: Float -> Float#
unboxFloat (F# f) = f
{-# INLINE unboxFloat #-}


unboxDouble :: Double -> Double#
unboxDouble (D# d) = d
{-# INLINE unboxDouble #-}


{-# RULES "foldRangeInt" 
    forall (get :: Int# -> Int) f r start end
    . foldRange get f r start end 
    = I# (foldRangeInt
                (\i     -> unboxInt (get i))
                (\d1 d2 -> unboxInt (f (I# d1) (I# d2)))
                (unboxInt r)
                start
                end)
 #-}


{-# RULES "foldRangeFloat" 
    forall (get :: Int# -> Float) f r start end
    . foldRange get f r start end 
    = F# (foldRangeFloat
                (\i     -> unboxFloat (get i))
                (\d1 d2 -> unboxFloat (f (F# d1) (F# d2)))
                (unboxFloat r)
                start
                end)
 #-}


{-# RULES "foldRangeDouble" 
    forall (get :: Int# -> Double) f r start end
    . foldRange get f r start end 
    = D# (foldRangeDouble
                (\i     -> unboxDouble (get i))
                (\d1 d2 -> unboxDouble (f (D# d1) (D# d2)))
                (unboxDouble r)
                start
                end)
 #-}

