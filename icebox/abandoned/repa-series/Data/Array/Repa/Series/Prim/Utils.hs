
module Data.Array.Repa.Series.Prim.Utils
        ( World, W
        , unwrapIO'
        , unwrapIO_
        , wrapIO1
        , wrapIO_)
where
import GHC.Types
import GHC.Exts

type World      
        = State# RealWorld

type W  = World


unwrapIO'  :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unwrapIO' (IO f) = f
{-# INLINE [1] unwrapIO' #-}


unwrapIO_  :: IO a -> State# RealWorld -> State# RealWorld
unwrapIO_ (IO f) world 
 = case f world of
        (# world', _ #) -> world'
{-# INLINE [1] unwrapIO_ #-}


wrapIO1    :: (World -> (# World, a #)) -> IO a
wrapIO1 = IO
{-# INLINE [1] wrapIO1 #-}


wrapIO_    :: (World -> World) -> IO ()
wrapIO_ f
        = IO (\w -> let !w' = f w in (# w', () #))
{-# INLINE [1] wrapIO_ #-}
