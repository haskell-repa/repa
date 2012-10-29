
module Data.Array.Repa.Flow.Generate
        ( generate
        , replicate
        , enumFromN)
where
import Data.Array.Repa.Flow.Base
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude hiding (replicate)

------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying the function to each
--   index.
generate :: Int -> (Int -> a) -> IO (Flow a)
generate (I# len) f
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 0

        let
         getSize _
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                return  $ Exact (len -# ix)

         get1 push1
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                if ix >=# len
                 then    push1 Nothing
                 else do UM.unsafeWrite refCount 0 (I# (ix +# 1#))
                         push1 $ Just (f (I# ix))

         get8 push8
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                if ix +# 8# ># len
                 then   push8 $ Right 1
                 else do
                        UM.unsafeWrite refCount 0 (I# (ix +# 8#))
                        push8 $ Left    ( f (I# (ix +# 0#))
                                        , f (I# (ix +# 1#))
                                        , f (I# (ix +# 2#))
                                        , f (I# (ix +# 3#))
                                        , f (I# (ix +# 4#))
                                        , f (I# (ix +# 5#))
                                        , f (I# (ix +# 6#))
                                        , f (I# (ix +# 7#)))

        return $ Flow getSize get1 get8
{-# INLINE [1] generate #-}


-- | Produce an flow of the given length with the same value in each position.
replicate :: Int -> a -> IO (Flow a)
replicate n x
        = generate n (const x)
{-# INLINE [1] replicate #-}


-- | Yield a vector of the given length containing values @x@, @x+1@ etc.
enumFromN :: (U.Unbox a, Num a, Show a) => a -> Int -> IO (Flow a)
enumFromN start (I# len)
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 (I# len)

        refAcc   <- UM.unsafeNew 1
        UM.unsafeWrite refAcc   0 start

        let
         getSize _
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                return  $ Exact (len -# count)

         get1 push1
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count ==# 0#
                 then   push1 Nothing
                 else do 
                        UM.unsafeWrite refCount 0 (I# (count -# 1#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 1)

                        push1 $ Just acc

         get8 push8
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count <=# 8#
                 then   push8 (Right 1)
                 else do
                        UM.unsafeWrite refCount 0 (I# (count -# 8#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 8)

                        push8 $ Left    ( acc
                                        , acc + 1
                                        , acc + 2
                                        , acc + 3
                                        , acc + 4
                                        , acc + 5
                                        , acc + 6
                                        , acc + 7)

        return $ Flow getSize get1 get8
{-# INLINE [1] enumFromN #-}
