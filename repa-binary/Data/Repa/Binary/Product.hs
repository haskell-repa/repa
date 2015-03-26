
module Data.Repa.Binary.Product
        ((:*:)(..))
where
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Generic.Mutable    as M

-- | Strict product type, written infix.
data a :*: b    
        = !a :*: !b             
        deriving (Eq, Show)

infixr :*:

-- Unboxed ----------------------------------------------------------------------------------------
-- Unboxed instance adapted from:
-- http://code.haskell.org/vector/internal/unbox-tuple-instances
--
data instance U.Vector (a :*: b)
  = V_Prod 
        {-# UNPACK #-} !Int 
        !(U.Vector a)
        !(U.Vector b)


instance (U.Unbox a, U.Unbox b) 
       => U.Unbox (a :*: b)


data instance U.MVector s (a :*: b)
  = MV_Prod {-# UNPACK #-} !Int 
        !(U.MVector s a)
        !(U.MVector s b)


instance (U.Unbox a, U.Unbox b) 
      => M.MVector U.MVector (a :*: b) where

  basicLength (MV_Prod n_ _as _bs) = n_
  {-# INLINE basicLength  #-}

  basicUnsafeSlice i_ m_ (MV_Prod _n_ as bs)
   = MV_Prod m_ (M.basicUnsafeSlice i_ m_ as)
                (M.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeSlice  #-}

  basicOverlaps (MV_Prod _n_1 as1 bs1) (MV_Prod _n_2 as2 bs2)
   =  M.basicOverlaps as1 as2
   || M.basicOverlaps bs1 bs2
  {-# INLINE basicOverlaps  #-}

  basicUnsafeNew n_
   = do as <- M.basicUnsafeNew n_
        bs <- M.basicUnsafeNew n_
        return $ MV_Prod n_ as bs
  {-# INLINE basicUnsafeNew  #-}

  basicUnsafeReplicate n_ (a :*: b)
   = do as <- M.basicUnsafeReplicate n_ a
        bs <- M.basicUnsafeReplicate n_ b
        return $ MV_Prod n_ as bs
  {-# INLINE basicUnsafeReplicate  #-}

  basicUnsafeRead (MV_Prod _n_ as bs) i_
   = do a <- M.basicUnsafeRead as i_
        b <- M.basicUnsafeRead bs i_
        return (a :*: b)
  {-# INLINE basicUnsafeRead  #-}

  basicUnsafeWrite (MV_Prod _n_ as bs) i_ (a :*: b)
   = do M.basicUnsafeWrite as i_ a
        M.basicUnsafeWrite bs i_ b
  {-# INLINE basicUnsafeWrite  #-}

  basicClear (MV_Prod _n_ as bs)
   = do M.basicClear as
        M.basicClear bs
  {-# INLINE basicClear  #-}

  basicSet (MV_Prod _n_ as bs) (a :*: b)
   = do M.basicSet as a
        M.basicSet bs b
  {-# INLINE basicSet  #-}

  basicUnsafeCopy (MV_Prod _n_1 as1 bs1) (MV_Prod _n_2 as2 bs2)
   = do M.basicUnsafeCopy as1 as2
        M.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeCopy  #-}

  basicUnsafeMove (MV_Prod _n_1 as1 bs1) (MV_Prod _n_2 as2 bs2)
   = do M.basicUnsafeMove as1 as2
        M.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeMove  #-}

  basicUnsafeGrow (MV_Prod n_ as bs) m_
   = do as' <- M.basicUnsafeGrow as m_
        bs' <- M.basicUnsafeGrow bs m_
        return $ MV_Prod (m_ + n_) as' bs'
  {-# INLINE basicUnsafeGrow  #-}


instance  (U.Unbox a, U.Unbox b) 
        => G.Vector U.Vector (a :*: b) where
  basicUnsafeFreeze (MV_Prod n_ as bs)
   = do as' <- G.basicUnsafeFreeze as
        bs' <- G.basicUnsafeFreeze bs
        return $ V_Prod n_ as' bs'
  {-# INLINE basicUnsafeFreeze  #-}

  basicUnsafeThaw (V_Prod n_ as bs)
   = do as' <- G.basicUnsafeThaw as
        bs' <- G.basicUnsafeThaw bs
        return $ MV_Prod n_ as' bs'
  {-# INLINE basicUnsafeThaw  #-}

  basicLength (V_Prod n_ _as _bs) 
   = n_
  {-# INLINE basicLength  #-}

  basicUnsafeSlice i_ m_ (V_Prod _n_ as bs)
   = V_Prod m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeSlice  #-}

  basicUnsafeIndexM (V_Prod _n_ as bs) i_
   = do a <- G.basicUnsafeIndexM as i_
        b <- G.basicUnsafeIndexM bs i_
        return (a :*: b)
  {-# INLINE basicUnsafeIndexM  #-}

  basicUnsafeCopy (MV_Prod _n_1 as1 bs1) (V_Prod _n_2 as2 bs2)
   = do G.basicUnsafeCopy as1 as2
        G.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeCopy  #-}

  elemseq _ (a :*: b)
      = G.elemseq (undefined :: U.Vector a) a
      . G.elemseq (undefined :: U.Vector b) b
  {-# INLINE elemseq  #-}

