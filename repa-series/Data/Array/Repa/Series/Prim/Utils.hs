
module Data.Array.Repa.Series.Prim.Utils
        ( World
        , unwrapIO'
        , unwrapIO_)
where
import GHC.Types
import GHC.Exts

type World      
        = State# RealWorld

unwrapIO'  :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unwrapIO' (IO f) = f
{-# INLINE unwrapIO' #-}

unwrapIO_  :: IO a -> State# RealWorld -> State# RealWorld
unwrapIO_ (IO f) world 
 = case f world of
        (# world', _ #) -> world'
{-# INLINE unwrapIO_ #-}


