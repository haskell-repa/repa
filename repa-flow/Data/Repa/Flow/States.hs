{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Flow.States
        ( Next   (..)
        , States (..)
        , Refs   (..)
        , foldRefsM)
where
import Control.Monad
import qualified Data.Vector.Mutable            as VM
#include "repa-stream.h"


-------------------------------------------------------------------------------
class (Ord i, Eq i) => Next i where

 -- | Get the zero for this index type.
 first     :: i

 -- | Given an index an arity, get the next index after this one,
 --   or `Nothing` if there aren't any more.
 next      :: i -> i -> Maybe i


-- | Unit indices.
instance Next () where

 first      = ()
 {-# INLINE first #-}

 next _ _   = Nothing
 {-# INLINE next #-}


-- | Integer indices.
instance Next Int where

 first   = 0
 {-# INLINE first #-}

 next i len
  | i + 1 >= len = Nothing
  | otherwise    = Just (i + 1)
 {-# INLINE next #-}


-- | Tuple indices.
instance Next (Int, Int) where

 first = (0, 0)
 {-# INLINE first #-}

 next (ix1, ix0) (a1, a0)
  | ix0 + 1 >= a0
  = if ix1 + 1 >= a1
        then Nothing
        else Just (ix1 + 1, 0)

  | otherwise
  = Just (ix1, ix0 + 1)
 {-# INLINE next #-}


-------------------------------------------------------------------------------
class (Ord i, Next i, Monad m) => States i m where

 -- | A collection of mutable references.
 data Refs i m a

 -- | Get the extent of the collection.
 extentRefs :: Refs i m a -> i

 -- | Allocate a new state of the given arity, also returning an index to the
 --   first element of the collection.
 newRefs    :: i -> a -> m (Refs i m a)

 -- | Write an element of the state.
 readRefs   :: Refs i m a -> i -> m a

 -- | Read an element of the state.
 writeRefs  :: Refs i m a -> i -> a -> m ()


-- | Fold all the elements in a collection of refs.
foldRefsM 
        :: States i m 
        => (a -> b -> b) -> b -> Refs i m a -> m b

foldRefsM f z refs
 = loop_foldsRefsM first z
 where
        loop_foldsRefsM i acc
         = do   x       <- readRefs refs i
                let acc' =  f x acc
                case next i (extentRefs refs) of
                 Nothing        -> return acc
                 Just i'        -> loop_foldsRefsM i' acc'
        {-# INLINE loop_foldsRefsM #-}       
{-# INLINE foldRefsM #-}


instance States Int IO where
 data Refs Int IO a             = Refs !(VM.IOVector a)
 extentRefs (Refs !refs)        = VM.length refs
 newRefs   !n !x                = liftM Refs $ unsafeNewWithVM n x
 readRefs  (Refs !refs) !i      = VM.unsafeRead  refs i
 writeRefs (Refs !refs) !i !x   = VM.unsafeWrite refs i x
 {-# NOINLINE newRefs #-}
 {-# INLINE readRefs #-}
 {-# INLINE writeRefs #-}


instance States Int m => States () m  where

 data Refs () m a               = URefs !(Refs Int m a)

 extentRefs _                   = ()
 {-# INLINE extentRefs #-}

 newRefs _ !x                
  = do  refs    <- newRefs  (1 :: Int) x
        return  $ URefs refs
 {-# NOINLINE newRefs #-}

 readRefs  (URefs !refs) _      = readRefs  refs 0
 writeRefs (URefs !refs) _ !x   = writeRefs refs 0 x
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

