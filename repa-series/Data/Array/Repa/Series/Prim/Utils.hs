
module Data.Array.Repa.Series.Prim.Utils
        ( World
        , unwrapIO'
        , unwrapIO_
        , wrapIO1)
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

wrapIO1    :: (World -> (# World, a #)) -> IO a
wrapIO1 = IO
{-# INLINE wrapIO1 #-}

