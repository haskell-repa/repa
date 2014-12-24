
module Data.Repa.Flow.States
        (States (..))
where
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector.Unboxed.Mutable    as UM


-- | An abstract collection of state values, indexed by a value of type @i@.
class Monad m => States i m a where
 data Refs i m a

 -- | Allocate a new state of the given arity.
 newRefs   :: i -> a -> m (Refs i m a)

 -- | Write an element of the state.
 readRefs  :: Refs i m a -> i -> m a

 -- | Read an element of the state.
 writeRefs :: Refs i m a -> i -> a -> m ()


-- | Singleton state.
instance Unbox a => States () IO a where
 data Refs () IO a 
        = URefsUnit !(UM.IOVector a)

 newRefs _ x
  = do  vec     <- UM.unsafeNew 1
        UM.unsafeWrite vec 0 x
        return $ URefsUnit vec

 readRefs  (URefsUnit v) _    = UM.unsafeRead  v 0
 writeRefs (URefsUnit v) _ x  = UM.unsafeWrite v 0 x


-- | States indexed by an integer.
instance Unbox a => States Int IO a where
 data Refs Int IO a
        = URefsInt  !(UM.IOVector a) 

 newRefs n x
  = do  vec     <- UM.unsafeNew n

        let loop_newRefs !i
             | i >= n    = return ()
             | otherwise 
             = do UM.unsafeWrite vec i x
                  loop_newRefs (n + 1)

        loop_newRefs 0
        return  $ URefsInt vec

