
module Data.Array.Repa.Bulk.Par.Reduction
        ( foldAll
        , foldInner)
where
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Bulk.Gang
import GHC.Exts
import qualified Data.Vector                            as V
import qualified Data.Array.Repa.Bulk.Seq.Reduction     as Seq


-- | Parallel tree reduction of an array to a single value. Each thread takes an
--   equally sized chunk of the data and computes a partial sum. The main thread
--   then reduces the array of partial sums to the final result.
--
--   We don't require that the initial value be a neutral element, so each thread
--   computes a fold1 on its chunk of the data, and the seed element is only
--   applied in the final reduction step.
--
foldAll :: Gang                -- ^ Gang to run the operation on.
        -> (Int# -> a)         -- ^ Function to get an element from the source.
        -> (a -> a -> a)       -- ^ Binary associative combining function.
        -> a                   -- ^ Starting value.
        -> Int#                -- ^ Number of elements.
        -> IO a

foldAll !gang f c !r !len
 | len ==# 0#   = return r
 | otherwise   
 = do   mvec    <- vnew (I# chunks)

        gangIO gang
         $ \tid -> fill mvec tid (split tid) (split (tid +# 1#))

        vec     <- vfreeze mvec
        return  $! V.foldl' c r vec
  where
        !threads    = gangSize gang
        !step       = (len +# threads -# 1#) `quotInt#` threads
        chunks      = ((len +# step -# 1#)   `quotInt#` step) `min#` threads

        split !ix   = len `min#` (ix *# step)
        {-# INLINE split #-}

        min# x y
         = if x <=# y 
                then x
                else y
        {-# INLINE min# #-}

        fill !mvec !tid !start !end
         | start >=# end = return ()
         | otherwise    
         = let  !x      = Seq.foldRange f c (f start) (start +# 1#) end
           in   vwrite mvec (I# tid) x
        {-# INLINE fill #-}

{-# INLINE [1] foldAll #-}


-- | Parallel reduction of a multidimensional array along the innermost dimension.
--   Each output value is computed by a single thread, with the output values
--   distributed evenly amongst the available threads.
foldInner 
        :: Gang                 -- ^ Gang to run the operation on.
        -> (Int# -> a -> IO ()) -- ^ Function to write into the result buffer.
        -> (Int# -> a)          -- ^ Function to get an element from the source.
        -> (a -> a -> a)        -- ^ Binary associative combination operator.
        -> a                    -- ^ Neutral starting value.
        -> Int#                 -- ^ Total length of source.
        -> Int#                 -- ^ Inner dimension (length to fold over).
        -> IO ()

foldInner gang write f c !r !len !n
 = gangIO gang
 $ \tid -> fill (split tid) (split (tid +# 1#))
  where
        !threads = gangSize gang
        !step    = (len +# threads -# 1#) `quotInt#` threads

        split !ix 
         = let !ix' = ix *# step
           in  if len <# ix' 
                then len
                else ix'
        {-# INLINE split #-}

        fill !start !end 
         = iter start (start *# n)
         where
          iter !sh !sz 
           | sh >=# end = return ()
           | otherwise 
           = do let !next = sz +# n
                write sh (Seq.foldRange f c r sz next)
                iter (sh +# 1#) next
          {-# INLINE iter #-}
        {-# INLINE fill #-}

{-# INLINE [1] foldInner #-}

