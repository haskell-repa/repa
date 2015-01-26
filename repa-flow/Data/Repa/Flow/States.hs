{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Flow.States
        ( Index   (..), Ix   (..)
        , States  (..), Refs (..))
where
import Control.Monad
import qualified Data.Vector.Mutable            as VM
#include "repa-stream.h"


-------------------------------------------------------------------------------
class (Ord i, Eq i, Eq (Ix i)) => Index i where
 -- | An index into the collection of states. 
 -- 
 --   The representation also carries the size of the collection, 
 --   and the only functions which produce Ix values ensure that 
 --   they are in-bounds.
 data Ix i

 -- | Get the zero index for this arity.
 zero      :: i -> Ix i

 -- | Take `Just` the index of the next state after this one,
 --   or `Nothing` if there aren't any more.
 next      :: Ix i -> Maybe (Ix i)

 -- | Construct `Just` a new index of the given value, for the same collection
 --   of states as the provided one. Yields `Nothing` if the index would be
 --   out-of-bounds.
 like      :: i -> Ix i -> Maybe (Ix i)

 -- | Check that an index is strictly less than the given arity.
 check     :: i -> i -> Maybe (Ix i)


-- | Unit indices.
instance Index () where

 data Ix () = UIx
 zero _     = UIx
 {-# INLINE zero #-}

 next _     = Nothing
 {-# INLINE next #-}

 like _ _   = Just UIx
 {-# INLINE like #-}

 check _ _  = Just UIx
 {-# INLINE check #-}

deriving instance Eq   (Ix ())
deriving instance Show (Ix ())


-- | Integer indices.
instance Index Int where

 data Ix Int            
        = IIx !Int !Int

 zero i = IIx 0 i
 {-# INLINE zero #-}

 next (IIx i len)
  | i + 1 >= len = Nothing
  | otherwise    = Just $ IIx (i + 1) len
 {-# INLINE next #-}

 like i (IIx _ len)
  | i >= len     = Nothing
  | otherwise    = Just $ IIx i len
 {-# INLINE like #-}

 check i n
  | i >= n       = Nothing
  | otherwise    = Just $ IIx i n
 {-# INLINE check #-}

deriving instance Eq   (Ix Int)
deriving instance Show (Ix Int)


-------------------------------------------------------------------------------
class (Index i, Monad m) => States i m where

 -- | A collection of mutable references.
 data Refs i m a

 -- | Allocate a new state of the given arity, also returning an index to the
 --   first element of the collection.
 newRefs   :: i -> a -> m (Refs i m a)

 -- | Write an element of the state.
 readRefs  :: Refs i m a -> Ix i -> m a

 -- | Read an element of the state.
 writeRefs :: Refs i m a -> Ix i -> a -> m ()


instance States Int IO where
 data Refs Int IO a                     = Refs !(VM.IOVector a)
 newRefs   !n !x                        = liftM Refs $ unsafeNewWithVM n x
 {-# NOINLINE newRefs #-}

 readRefs  (Refs !refs) (IIx !i _)      = VM.unsafeRead  refs i
 {-# INLINE readRefs #-}

 writeRefs (Refs !refs) (IIx !i _) !x   = VM.unsafeWrite refs i x
 {-# INLINE writeRefs #-}


instance States Int m => States () m  where

 data Refs () m a                       = URefs !(Refs Int m a)

 newRefs _ !x                
  = do  refs    <- newRefs  (1 :: Int) x
        return  $ URefs refs
 {-# NOINLINE newRefs #-}

 readRefs  (URefs !refs) _              = readRefs  refs (zero 1)
 writeRefs (URefs !refs) _ !x           = writeRefs refs (zero 1) x
 {-# INLINE readRefs #-}
 {-# INLINE writeRefs #-}


-------------------------------------------------------------------------------
unsafeNewWithVM :: Int -> a -> IO (VM.IOVector a)
unsafeNewWithVM n x
 = do   vec     <- VM.unsafeNew n

        let loop_newRefs !i
             | i >= n    = return ()
             | otherwise 
             = do VM.unsafeWrite vec i x
                  loop_newRefs (i + 1)
            {-# INLINE loop_newRefs #-}

        loop_newRefs 0
        return vec
{-# INLINE unsafeNewWithVM #-}

