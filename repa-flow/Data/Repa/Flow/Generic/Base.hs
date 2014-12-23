
module Data.Repa.Flow.Generic.Base
        ( Sources       (..)
        , Sinks         (..)
        , States        (..)
        , reSource)
where
import Control.Monad
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector.Unboxed.Mutable    as UM


data Sources i m e
        = Sources
        { sourceArity   :: i
        , sourcePull    :: i -> (e -> m ()) -> m () -> m () }


data Sinks   i m e
        = Sinks
        { sinkArity     :: i
        , sinkPush      :: i -> e -> m ()
        , sinkEject     :: i -> m () }


-------------------------------------------------------------------------------
-- | An abstract collection of state values, indexed by a value of type @i@.
class Monad m => States i m a where
 data Refs i m a

 newRefs   :: i -> a -> m (Refs i m a)
 readRefs  :: Refs i m a -> i -> m a
 writeRefs :: Refs i m a -> i -> a -> m ()


instance Unbox a => States () IO a where
 data Refs () IO a 
        = URefs (UM.IOVector a)

 newRefs _ x
  = do  vec     <- UM.unsafeNew 1
        UM.unsafeWrite vec 0 x
        return $ URefs vec

 readRefs  (URefs v) _    = UM.unsafeRead  v 0
 writeRefs (URefs v) _ x  = UM.unsafeWrite v 0 x


-------------------------------------------------------------------------------
reSource :: (i1 -> i2) 
         -> (i2 -> i1)
         -> Sources i1 m a -> Sources i2 m a
reSource from to (Sources n1 pullX)
 = Sources (from n1) pull_reSource
 where  pull_reSource i2 pull eject 
         = pullX (to i2) pull eject
        {-# INLINE pull_reSource #-}
{-# INLINE [2] reSource #-}



        
        
