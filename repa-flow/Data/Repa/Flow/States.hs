{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Flow.States
        ( Index  (..), Ix   (..)
        , States (..), Refs (..)
        , Box    (..))
where
import Control.Monad
import Data.Vector.Unboxed                      (Unbox)
import qualified Data.Vector.Mutable            as VM
import qualified Data.Vector.Unboxed.Mutable    as UM


-------------------------------------------------------------------------------
class Ord i => Index i where
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


-- | Integer indices.
instance Index Int where

 data Ix Int            
        = IIx !Int !Int

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

 check i n
  | i >= n      = Nothing
  | otherwise   = Just $ IIx i n
 {-# INLINE check #-}


-------------------------------------------------------------------------------
-- | An collection of state values, indexed by a value of type @i@.
class (Index i, Monad m) => States i m a where


 -- | A collection of mutable references.
 data Refs i m a


 -- | Allocate a new state of the given arity, also returning an index to the
 --   first element of the collection.
 newRefs   :: i -> a -> m (Refs i m a)

 -- | Write an element of the state.
 readRefs  :: Refs i m a -> Ix i -> m a

 -- | Read an element of the state.
 writeRefs :: Refs i m a -> Ix i -> a -> m ()


-------------------------------------------------------------------------------
instance States Int m a 
      => States ()  m a where
 data Refs () m a           = URefs !(Refs Int m a)

 newRefs _ x                
  = do  refs    <- newRefs  (1 :: Int) x
        return  $ URefs refs
 {-# INLINE newRefs #-}

 readRefs  (URefs refs) _   = readRefs  refs (zero 1)
 writeRefs (URefs refs) _ x = writeRefs refs (zero 1) x
 {-# INLINE readRefs #-}
 {-# INLINE writeRefs #-}

-------------------------------------------------------------------------------
-- | Data type to indicate that a state value is explicitly boxed.
data Box a = Box !a

instance States Int IO (Box a) where
 data Refs Int IO (Box a)              = BRefs !(VM.IOVector a)
 newRefs n (Box x)                     = liftM BRefs $ unsafeNewWithVM n x
 readRefs  (BRefs v) (IIx i _)         = liftM Box   $ VM.unsafeRead v i
 writeRefs (BRefs v) (IIx i _) (Box x) = VM.unsafeWrite v i x
 {-# INLINE newRefs #-}
 {-# INLINE readRefs #-}
 {-# INLINE writeRefs #-}


-------------------------------------------------------------------------------
instance States Int IO Int where
 data Refs Int IO Int                  = IRefs !(UM.IOVector Int) 
 newRefs n x                           = liftM IRefs $ unsafeNewWithUM n x
 readRefs  (IRefs v) (IIx i _)         = UM.unsafeRead v i
 writeRefs (IRefs v) (IIx i _) x       = UM.unsafeWrite v i x
 {-# INLINE newRefs #-}
 {-# INLINE readRefs #-}
 {-# INLINE writeRefs #-}


-------------------------------------------------------------------------------
instance (States i m a, States i m b)
      =>  States i m (a, b) where
 data Refs i m (a, b)            
  = RefsT2 !(Refs i m a) !(Refs i m b)

 newRefs n (x1, x2)   
  = do  refs1   <- newRefs n x1
        refs2   <- newRefs n x2
        return  $ RefsT2 refs1 refs2
 {-# INLINE newRefs #-}

 readRefs  (RefsT2 r1 r2) ix
  = do  x1      <- readRefs r1 ix
        x2      <- readRefs r2 ix
        return  (x1, x2)
 {-# INLINE readRefs #-}

 writeRefs (RefsT2 ra rb) ix (x1, x2) 
  = do  writeRefs ra ix x1
        writeRefs rb ix x2
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


unsafeNewWithUM :: Unbox a => Int -> a -> IO (UM.IOVector a)
unsafeNewWithUM n x
 = do   vec     <- UM.unsafeNew n

        let loop_newRefs !i
             | i >= n    = return ()
             | otherwise 
             = do UM.unsafeWrite vec i x
                  loop_newRefs (i + 1)
            {-# INLINE loop_newRefs #-}

        loop_newRefs 0
        return vec
{-# INLINE unsafeNewWithUM #-}


