
module Data.Array.Repa.Chain.Eval
        ( unchain
        , evalM)
where
import Data.Array.Repa.Chain.Base
import Data.Vector.Unboxed                      (Unbox, Vector)
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad.ST
import GHC.Exts


unchain :: Unbox a => Chain a -> Vector a
unchain c@(Chain len s0 next)
 = runST 
 $ do   !mvec    <- UM.unsafeNew (I# len)

        let write ix x 
                = UM.unsafeWrite mvec (I# ix) x
            {-# INLINE write #-}

        _       <- evalM write c

        U.unsafeFreeze mvec
{-# INLINE [1] unchain #-}


evalM   :: Monad m 
        => (Int# -> a -> m ())
        -> Chain a
        -> m ()

evalM write (Chain len !s0 next)
 = go 0# s0

 where  -- NOTE: Don't put an INLINE pragma here or GHC will 
        --       eta-expand the call to 'go' in a bad way involving
        --       casts.
        go ix s
               | ix >=# len   = return ()
               | otherwise
               = case next ix s of
                        Yield s' x
                         -> do  write ix x
                                go (ix +# 1#) s'

                        Update s'
                         ->     go ix s'
{-# INLINE [1] evalM #-}
