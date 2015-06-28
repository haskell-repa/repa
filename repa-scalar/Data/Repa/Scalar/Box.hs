
module Data.Repa.Scalar.Box
        ( Box           (..))
where
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Generic.Mutable    as GM


-- | Strict, boxed wrapper for a value.
--
--   Useful as a default case when defining instances for polytypic 
--   data types.
newtype Box a
        = Box a
        deriving (Eq, Show)


-- Unboxed ----------------------------------------------------------------------------------------
-- Unboxed instance adapted from:
-- http://code.haskell.org/vector/internal/unbox-tuple-instances
--
data instance U.Vector (Box a)
  = V_Box
        {-# UNPACK #-} !Int 
        !(V.Vector a)


data instance U.MVector s (Box a)
  = MV_Box
        {-# UNPACK #-} !Int 
        !(V.MVector s a)


instance U.Unbox a
      => GM.MVector U.MVector (Box a) where

  basicLength (MV_Box n _as) = n
  {-# INLINE basicLength  #-}

  basicUnsafeSlice i m (MV_Box _n as)
   =  MV_Box m (GM.basicUnsafeSlice i m as)
  {-# INLINE basicUnsafeSlice  #-}

  basicOverlaps (MV_Box _n1 as1) (MV_Box _n2 as2)
   =  GM.basicOverlaps as1 as2
  {-# INLINE basicOverlaps  #-}

  basicUnsafeNew n
   = do as <- GM.basicUnsafeNew n
        return $ MV_Box n as
  {-# INLINE basicUnsafeNew  #-}

  basicUnsafeReplicate n (Box a)
   = do as <- GM.basicUnsafeReplicate n a
        return $ MV_Box n as
  {-# INLINE basicUnsafeReplicate  #-}

  basicUnsafeRead (MV_Box _n as) i
   = do v  <- GM.basicUnsafeRead as i
        return $ Box v
  {-# INLINE basicUnsafeRead  #-}

  basicUnsafeWrite (MV_Box _n as) i (Box a)
   = a `seq` GM.basicUnsafeWrite as i a
  {-# INLINE basicUnsafeWrite  #-}

  basicClear (MV_Box _n as)
   =    GM.basicClear as
  {-# INLINE basicClear  #-}

  basicSet   (MV_Box _n as) (Box a)
   =    GM.basicSet as a
  {-# INLINE basicSet  #-}

  basicUnsafeCopy (MV_Box _n1 as1) (MV_Box  _n2 as2)
   =    GM.basicUnsafeCopy as1 as2
  {-# INLINE basicUnsafeCopy  #-}

  basicUnsafeMove (MV_Box _n1 as1) (MV_Box _n2 as2)
   =    GM.basicUnsafeMove as1 as2
  {-# INLINE basicUnsafeMove  #-}

  basicUnsafeGrow (MV_Box n as) m
   = do as' <- GM.basicUnsafeGrow as m
        return $ MV_Box (m + n) as'
  {-# INLINE basicUnsafeGrow  #-}


instance U.Unbox a
      => G.Vector U.Vector (Box a) where

  basicUnsafeFreeze (MV_Box n as)
   = do as' <- G.basicUnsafeFreeze as
        return $ V_Box n as'
  {-# INLINE basicUnsafeFreeze  #-}

  basicUnsafeThaw (V_Box n as)
   = do as' <- G.basicUnsafeThaw as
        return $ MV_Box n as'
  {-# INLINE basicUnsafeThaw  #-}

  basicLength (V_Box n _as) 
   = n
  {-# INLINE basicLength  #-}

  basicUnsafeSlice i m (V_Box _n as)
   = V_Box m (G.basicUnsafeSlice i m as)
  {-# INLINE basicUnsafeSlice  #-}

  basicUnsafeIndexM (V_Box _n as) i
   = do a <- G.basicUnsafeIndexM as i
        return (Box a)
  {-# INLINE basicUnsafeIndexM  #-}

  basicUnsafeCopy (MV_Box _n1 as1) (V_Box _n2 as2)
   =    G.basicUnsafeCopy as1 as2
  {-# INLINE basicUnsafeCopy  #-}

  elemseq _ (Box a)
      = G.elemseq (undefined :: V.Vector a) a
  {-# INLINE elemseq  #-}

