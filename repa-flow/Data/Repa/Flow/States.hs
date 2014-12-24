
module Data.Repa.Flow.States
        ( States (..)
        , Ix     (..)
        , Refs   (..))
where
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector.Unboxed.Mutable    as UM


-------------------------------------------------------------------------------
-- | An collection of state values, indexed by a value of type @i@.
class (Ord i, Monad m) => States i m a where

 -- | An index into the collection of states. 
 -- 
 --   The representation also carries the size of the collection, 
 --   and the only functions which produce Ix values ensure that 
 --   they are in-bounds.
 data Ix i

 -- | A collection of mutable references.
 data Refs i m a

 -- | Get the zero index for this arity.
 zero      :: i -> Ix i

 -- | Take `Just` the index of the next state after this one,
 --   or `Nothing` if there aren't any more.
 next      :: Ix i -> Maybe (Ix i)

 -- | Construct `Just` a new index of the given value, for the same collection
 --   of states as the provided one. Yields `Nothing` if the index would be
 --   out-of-bounds.
 like      :: i -> Ix i -> Maybe (Ix i)

 -- | Allocate a new state of the given arity, also returning an index to the
 --   first element of the collection.
 newRefs   :: i -> a -> m (Refs i m a)

 -- | Write an element of the state.
 readRefs  :: Refs i m a -> Ix i -> m a

 -- | Read an element of the state.
 writeRefs :: Refs i m a -> Ix i -> a -> m ()


-------------------------------------------------------------------------------
-- | Singleton state.
instance Unbox a => States () IO a where
 data Ix ()             = UIx
 data Refs () IO a      = URefs !(UM.IOVector a)

 zero _    = UIx
 {-# INLINE zero #-}

 next _    = Nothing
 {-# INLINE next #-}

 like _ _  = Just UIx
 {-# INLINE like #-}

 newRefs _ x
  = do  vec     <- UM.unsafeNew 1
        UM.unsafeWrite vec 0 x
        return  $ URefs vec
 {-# INLINE newRefs #-}

 readRefs  (URefs v) _    = UM.unsafeRead  v 0
 {-# INLINE readRefs #-}

 writeRefs (URefs v) _ x  = UM.unsafeWrite v 0 x
 {-# INLINE writeRefs #-}


-------------------------------------------------------------------------------
-- | States indexed by an integer.
instance Unbox a => States Int IO a where
 data Ix Int            = IIx   !Int !Int
 data Refs Int IO a     = IRefs !(UM.IOVector a) 

 zero i = IIx 0 i
 {-# INLINE zero #-}

 next (IIx i len)
  | i >= len    = Nothing
  | otherwise   = Just $ IIx (i + 1) len
 {-# INLINE next #-}

 like i (IIx _ len)
  | i >= len    = Nothing
  | otherwise   = Just $ IIx i len
 {-# INLINE like #-}

 newRefs n x
  = do  vec     <- UM.unsafeNew n

        let loop_newRefs !i
             | i >= n    = return ()
             | otherwise 
             = do UM.unsafeWrite vec i x
                  loop_newRefs (n + 1)
            {-# INLINE loop_newRefs #-}

        loop_newRefs 0
        return  $ IRefs vec
 {-# INLINE newRefs #-}

 readRefs  (IRefs v) (IIx i _)   = UM.unsafeRead v i
 {-# INLINE readRefs #-}

 writeRefs (IRefs v) (IIx i _) x = UM.unsafeWrite v i x
 {-# INLINE writeRefs #-}

