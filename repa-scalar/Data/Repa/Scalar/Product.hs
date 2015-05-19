{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Data.Repa.Scalar.Product
        ( -- * Product type
          (:*:)         (..)
        , IsProdList    (..)

          -- * Selecting
        , Select (..)

          -- * Discarding
        , Discard (..), Keep(..), Drop(..)

          -- * Masking
        , Mask    (..))
where
import Data.Repa.Scalar.Singleton.Nat
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Generic.Mutable    as M


---------------------------------------------------------------------------------------------------
-- | Strict product type, written infix.
data a :*: b    
        = !a :*: !b             
        deriving (Eq, Show)

infixr :*:


class IsProdList p where
 -- | Check if a sequence of products forms a valid list, 
 --   using () for the nil value.
 --
 -- @
 -- isProdList (1 :*: 4 :*: 5)  ... no instance
 --
 -- isProdList (1 :*: 4 :*: ()) = True
 -- @
 --
 isProdList :: p -> Bool

instance IsProdList () where
 isProdList _ = True
 {-# INLINE isProdList #-}


instance IsProdList fs => IsProdList (f :*: fs) where
 isProdList (_ :*: xs) = isProdList xs
 {-# INLINE isProdList #-}


---------------------------------------------------------------------------------------------------
class    IsProdList t
      => Select  (n :: N) t where
 type    Select'    n        t
 -- | Return just the given field in this tuple.
 select ::          Nat n -> t -> Select' n t


instance IsProdList ts
      => Select  Z    (t1 :*: ts) where
 type Select'    Z    (t1 :*: ts) = t1
 select       Zero    (t1 :*: _)  = t1
 {-# INLINE select #-}


instance Select n ts
      => Select (S n) (t1 :*: ts) where
 type Select'   (S n) (t1 :*: ts) = Select' n ts
 select      (Succ n) (_  :*: xs) = select  n xs
 {-# INLINE select #-}


---------------------------------------------------------------------------------------------------
class    IsProdList t 
      => Discard (n :: N) t where
 type    Discard'   n        t
 -- | Discard the given field in this tuple.
 discard ::         Nat n -> t -> Discard' n t


instance IsProdList ts 
      => Discard Z     (t1 :*: ts) where
 type Discard'   Z     (t1 :*: ts) = ts
 discard      Zero     (_  :*: xs) = xs
 {-# INLINE discard #-}


instance Discard n ts 
      => Discard (S n) (t1 :*: ts) where
 type Discard'   (S n) (t1 :*: ts) = t1 :*: Discard' n ts
 discard      (Succ n) (x1 :*: xs) = x1 :*: discard  n xs
 {-# INLINE discard #-}


---------------------------------------------------------------------------------------------------
-- | Singleton to indicate a field that should be dropped.
data Drop = Drop

-- | Singleton to indicate a field that should be kept.
data Keep = Keep


-- | Class of data types that can have parts masked out.
class (IsProdList m, IsProdList t) => Mask  m t where
 type Mask' m t
 -- | Mask out some component of a type.
 --
 -- @  
 -- mask (Keep :*: Drop  :*: Keep :*: ()) 
 --      (1    :*: \"foo\" :*: \'a\'  :*: ())   =   (1 :*: \'a\' :*: ())
 --
 -- mask (Drop :*: Drop  :*: Drop :*: ()) 
 --      (1    :*: \"foo\" :*: \'a\'  :*: ())   =   ()
 -- @
 --
 mask :: m -> t -> Mask' m t


instance Mask () () where
 type Mask' ()   () = ()
 mask       ()   () = ()
 {-# INLINE mask #-}


instance Mask ms ts 
      => Mask (Keep :*: ms) (t1 :*: ts) where
 type Mask'   (Keep :*: ms) (t1 :*: ts) = t1 :*: Mask' ms ts
 mask         (_    :*: ms) (x1 :*: xs) = x1 :*: mask  ms xs
 {-# INLINE mask #-}


instance Mask ms ts
      => Mask (Drop :*: ms) (t1 :*: ts) where
 type Mask'   (Drop :*: ms) (t1 :*: ts) = Mask' ms ts
 mask         (_    :*: ms) (_  :*: xs) = mask  ms xs
 {-# INLINE mask #-}


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

