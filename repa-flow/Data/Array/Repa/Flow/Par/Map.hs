
module Data.Array.Repa.Flow.Par.Map
        ( map
        , zip
        , zipWith
        , zipLeft
        , zipLeftWith)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Distro
import qualified Data.Array.Repa.Flow.Seq       as Seq
import Prelude hiding (map, zip, zipWith)
import GHC.Exts


-- | Apply a function to every element of a flow.
map     :: (a -> b) 
        -> Flow mode dist a -> Flow mode dist b

map f (Flow gang distro start frag)
 = Flow gang distro start frag'
 where  frag' state ix 
         = Seq.map f (frag state ix)
        {-# INLINE frag' #-}
{-# INLINE [2] map #-}


-- | Combine two flows into a flow of tuples, pulling one element at a time.
zip     :: Flow mode BB a -> Flow mode BB b 
        -> Flow mode BB (a, b)

zip (Flow gang1 distro1 start1 frag1) (Flow _ _ start2 frag2)
 = Flow gang1 distro1 start' frag'
 where  
        start'
         = do   state1  <- start1
                state2  <- start2
                return (state1, state2)
        {-# INLINE start' #-}

        frag' (state1, state2) ix 
         = Seq.zip (frag1 state1 ix) (frag2 state2 ix)
        {-# INLINE frag' #-}

{-# INLINE [2] zip #-}


-- | Combine two flows with a function, pulling one element at a time.
zipWith :: (a -> b -> c) 
        -> Flow mode BB a -> Flow mode BB b 
        -> Flow mode BB c

zipWith f flowA flowB
        = map (uncurry f) $ zip flowA flowB
{-# INLINE [2] zipWith #-}


-- | Pair elements of a flow with elements gained from some function.
zipLeft :: Flow mode BB a 
        -> (Int# -> b) 
        -> Flow mode BB (a, b)

zipLeft (Flow gang distro start frag) getB
 = Flow gang distro start frag'
 where  
        frag' state n
         = let  !start'  = distroBalancedFragStart distro n
                getB' ix = getB (ix +# start')
           in   Seq.zipLeft (frag state n) getB'
        {-# INLINE frag' #-}

{-# INLINE [2] zipLeft #-}


-- | Combine a flow with elements gained from some function.
zipLeftWith 
        :: (a -> b -> c) 
        -> Flow mode BB a -> (Int# -> b)
        -> Flow mode BB c

zipLeftWith f flowA getB
        = map (uncurry f) $ zipLeft flowA getB
{-# INLINE [2] zipLeftWith #-}

