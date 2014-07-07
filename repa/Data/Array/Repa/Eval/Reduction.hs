{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Array.Repa.Eval.Reduction
        ( foldS,    foldP
        , foldAllS, foldAllP)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Gang
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as M
import GHC.Base                                 ( quotInt, divInt )
import GHC.Exts

-- | Sequential reduction of a multidimensional array along the innermost dimension.
foldS :: (Elt a, V.Unbox a)
      => M.IOVector a   -- ^ vector to write elements into
      -> (Int# -> a)    -- ^ function to get an element from the given index
      -> (a -> a -> a)  -- ^ binary associative combination function
      -> a              -- ^ starting value (typically an identity)
      -> Int#           -- ^ inner dimension (length to fold over)
      -> IO ()
{-# INLINE [1] foldS #-}
foldS !vec get c !r !n
  = iter 0# 0#
  where
    !(I# end) = M.length vec

    {-# INLINE iter #-}
    iter !sh !sz 
     | 1# <- sh >=# end 
     = return ()

     | otherwise 
     = do let !next = sz +# n
          M.unsafeWrite vec (I# sh) (reduceAny get c r sz next)
          iter (sh +# 1#) next


-- | Parallel reduction of a multidimensional array along the innermost dimension.
--   Each output value is computed by a single thread, with the output values
--   distributed evenly amongst the available threads.
foldP :: (Elt a, V.Unbox a)
      => M.IOVector a   -- ^ vector to write elements into
      -> (Int -> a)     -- ^ function to get an element from the given index
      -> (a -> a -> a)  -- ^ binary associative combination operator 
      -> a              -- ^ starting value. Must be neutral with respect
                        -- ^ to the operator. eg @0 + a = a@.
      -> Int            -- ^ inner dimension (length to fold over)
      -> IO ()
{-# INLINE [1] foldP #-}
foldP vec f c !r (I# n)
  = gangIO theGang
  $ \(I# tid) -> fill (split tid) (split (tid +# 1#))
  where
    !(I# threads) = gangSize theGang
    !(I# len)     = M.length vec
    !step         = (len +# threads -# 1#) `quotInt#` threads

    {-# INLINE split #-}
    split !ix 
     = let !ix' = ix *# step
       in  case len <# ix' of
             0# -> ix'
             _  -> len

    {-# INLINE fill #-}
    fill !start !end 
     = iter start (start *# n)
     where
        {-# INLINE iter #-}
        iter !sh !sz 
         | 1# <- sh >=# end 
         = return ()

         | otherwise 
         = do   let !next = sz +# n
                M.unsafeWrite vec (I# sh) (reduce f c r (I# sz) (I# next))
                iter (sh +# 1#) next


-- | Sequential reduction of all the elements in an array.
foldAllS :: (Elt a, V.Unbox a)
         => (Int# -> a)         -- ^ function to get an element from the given index
         -> (a -> a -> a)       -- ^ binary associative combining function
         -> a                   -- ^ starting value
         -> Int#                -- ^ number of elements
         -> a

{-# INLINE [1] foldAllS #-}
foldAllS f c !r !len
 = reduceAny (\i -> f i) c r 0# len 



-- | Parallel tree reduction of an array to a single value. Each thread takes an
--   equally sized chunk of the data and computes a partial sum. The main thread
--   then reduces the array of partial sums to the final result.
--
--   We don't require that the initial value be a neutral element, so each thread
--   computes a fold1 on its chunk of the data, and the seed element is only
--   applied in the final reduction step.
--
foldAllP :: (Elt a, V.Unbox a)
         => (Int -> a)          -- ^ function to get an element from the given index
         -> (a -> a -> a)       -- ^ binary associative combining function
         -> a                   -- ^ starting value
         -> Int                 -- ^ number of elements
         -> IO a
{-# INLINE [1] foldAllP #-}

foldAllP f c !r !len
  | len == 0    = return r
  | otherwise   = do
      mvec <- M.unsafeNew chunks
      gangIO theGang $ \tid -> fill mvec tid (split tid) (split (tid+1))
      vec  <- V.unsafeFreeze mvec
      return $! V.foldl' c r vec
  where
    !threads    = gangSize theGang
    !step       = (len + threads - 1) `quotInt` threads
    chunks      = ((len + step - 1) `divInt` step) `min` threads

    {-# INLINE split #-}
    split !ix   = len `min` (ix * step)

    {-# INLINE fill #-}
    fill !mvec !tid !start !end
      | start >= end = return ()
      | otherwise    = M.unsafeWrite mvec tid (reduce f c (f start) (start+1) end)



-- Reduce ---------------------------------------------------------------------
-- | This is the primitive reduction function.
--   We use manual specialisations and rewrite rules to avoid the result
--   being boxed up in the final iteration.
{-# INLINE [0] reduce #-}
reduce  :: (Int -> a)           -- ^ Get data from the array.
        -> (a -> a -> a)        -- ^ Function to combine elements.
        -> a                    -- ^ Starting value.
        -> Int                  -- ^ Starting index in array.
        -> Int                  -- ^ Ending index in array.
        -> a                    -- ^ Result.
reduce f c !r (I# start) (I# end)
 = reduceAny (\i -> f (I# i)) c r start end


-- | Sequentially reduce values between the given indices
{-# INLINE [0] reduceAny #-}
reduceAny :: (Int# -> a) -> (a -> a -> a) -> a -> Int# -> Int# -> a
reduceAny f c !r !start !end 
 = iter start r
 where
   {-# INLINE iter #-}
   iter !i !z 
    | 1# <- i >=# end  = z 
    | otherwise        = iter (i +# 1#) (z `c` f i)


{-# INLINE [0] reduceInt #-}
reduceInt
        :: (Int# -> Int#)
        -> (Int# -> Int# -> Int#)
        -> Int# 
        -> Int# -> Int# 
        -> Int#

reduceInt f c !r !start !end 
 = iter start r
 where
   {-# INLINE iter #-}
   iter !i !z 
    | 1# <- i >=# end   = z 
    | otherwise         = iter (i +# 1#) (z `c` f i)


{-# INLINE [0] reduceFloat #-}
reduceFloat
        :: (Int# -> Float#) 
        -> (Float# -> Float# -> Float#)
        -> Float# 
        -> Int# -> Int# 
        -> Float#

reduceFloat f c !r !start !end 
 = iter start r
 where
   {-# INLINE iter #-}
   iter !i !z 
    | 1# <- i >=# end   = z 
    | otherwise         = iter (i +# 1#) (z `c` f i)


{-# INLINE [0] reduceDouble #-}
reduceDouble
        :: (Int# -> Double#) 
        -> (Double# -> Double# -> Double#)
        -> Double# 
        -> Int# -> Int# 
        -> Double#

reduceDouble f c !r !start !end 
 = iter start r
 where
   {-# INLINE iter #-}
   iter !i !z 
    | 1# <- i >=# end   = z 
    | otherwise         = iter (i +# 1#) (z `c` f i)


{-# INLINE unboxInt #-}
unboxInt :: Int -> Int#
unboxInt (I# i) = i


{-# INLINE unboxFloat #-}
unboxFloat :: Float -> Float#
unboxFloat (F# f) = f


{-# INLINE unboxDouble #-}
unboxDouble :: Double -> Double#
unboxDouble (D# d) = d


{-# RULES "reduceInt" 
    forall (get :: Int# -> Int) f r start end
    . reduceAny get f r start end 
    = I# (reduceInt 
                (\i     -> unboxInt (get i))
                (\d1 d2 -> unboxInt (f (I# d1) (I# d2)))
                (unboxInt r)
                start
                end)
 #-}


{-# RULES "reduceFloat" 
    forall (get :: Int# -> Float) f r start end
    . reduceAny get f r start end 
    = F# (reduceFloat
                (\i     -> unboxFloat (get i))
                (\d1 d2 -> unboxFloat (f (F# d1) (F# d2)))
                (unboxFloat r)
                start
                end)
 #-}


{-# RULES "reduceDouble" 
    forall (get :: Int# -> Double) f r start end
    . reduceAny get f r start end 
    = D# (reduceDouble 
                (\i     -> unboxDouble (get i))
                (\d1 d2 -> unboxDouble (f (D# d1) (D# d2)))
                (unboxDouble r)
                start
                end)
 #-}


