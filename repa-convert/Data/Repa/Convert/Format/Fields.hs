{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Convert.Format.Fields
        ( App (..)
        , Sep (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Binary
import Data.Word
import Data.Char
import qualified Foreign.Ptr                    as S


---------------------------------------------------------------------------------------------------
-- | Formatting fields.
instance (Format a, Format b) 
       => Format (a :*: b) where
 type Value (a :*: b) = Value a :*: Value b

 fieldCount (fa :*: fb)
  = do  ca      <- fieldCount fa 
        cb      <- fieldCount fb
        return  $ ca + cb
 {-# INLINE fieldCount #-}

 fixedSize  (fa :*: fb)
  = do  sa      <- fixedSize fa
        sb      <- fixedSize fb
        return  $  sa + sb
 {-# INLINE fixedSize #-}

 packedSize (fa :*: fb) (xa :*: xb)
  = do  sa      <- packedSize fa xa
        sb      <- packedSize fb xb
        return  $  sa + sb
 {-# INLINE packedSize #-}


-- | Packing fields.
instance (Packable fa, Packable fb) 
      =>  Packable (fa :*: fb) where

 pack   buf (fa :*: fb) (xa :*: xb) k
  =  pack buf                  fa xa $ \oa 
  -> pack (S.plusPtr buf oa)   fb xb $ \ob
  -> k (oa + ob)
 {-# INLINE pack #-}

 unpack buf (fa :*: fb) k
  =  unpack buf                fa    $ \(xa, oa)
  -> unpack (S.plusPtr buf oa) fb    $ \(xb, ob)
  -> k (xa :*: xb, oa + ob)
 {-# INLINE unpack #-}


-- Field Containers -------------------------------------------------------------------------------
-- | Append fields without separators.
data App f      = App f


-- | Type used internally to carry the separating character.
data CApp       = CApp


-- | Appended fields.
instance Format f 
      => Format (App f) where
 type Value (App f)     = Value f
 fieldCount _           = Nothing
 fixedSize  (App f)     = fixedSize f
 packedSize (App f) x   = packedSize f x
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


-- | Packing appended fields.
instance (Packable fa, Packables CApp fb)
       => Packables CApp (fa :*: fb) where
 packs buf sep (fa :*: fb) (xa :*: xb) k
  =  pack  buf                    fa xa $ \oa 
  -> packs (S.plusPtr buf oa) sep fb xb $ \ob
  -> k (oa + ob)
 {-# INLINE packs #-}

 unpack buf (fa :*: fb) k
  =  unpack buf                   fa    $ \(xa, oa)
  -> unpack (S.plusPtr buf oa)    fb    $ \(xb, ob)
  -> k (xa :*: xb, oa + ob)
 {-# INLINE unpack #-}


-- | Packing appended fields.
instance (Packable f1, Packable f2, Packables CApp f2) 
       => Packable (App (f1 :*: f2)) where
 pack buf (App f) x k = packs buf CApp f x k


-- Separated Fields -------------------------------------------------------------------------------
-- | Separate fields with the given character.
data Sep f      = Sep  Char f


-- | Type used internally to carry the separating character.
data CSep       = CSep Char


-- | Separated fields.
instance Format f
      => Format (Sep f) where
 type Value (Sep f)     = Value f
 fieldCount _           = Nothing

 fixedSize  (Sep _ f)
  = do  n       <- fieldCount f
        s       <- fixedSize f
        return  $ s + (if n == 0 then 0 else n - 1)

 packedSize (Sep _ f) x   
  = do  n       <- fieldCount f
        s       <- packedSize f x
        return  $ s + (if n == 0 then 0 else n - 1)
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


-- | Packing separated fields.
instance (Packable f1, Packable f2, Packables CSep f2) 
       => Packable (Sep (f1 :*: f2)) where
 pack buf (Sep c f) x k = packs buf (CSep c) f x k


-- | Packing separated fields.
instance (Packable fa, Packables CSep fb)
       => Packables CSep    (fa :*: fb) where
 packs   buf sep@(CSep c) (fa :*: fb) (xa :*: xb) k
  =  pack  buf                            fa       xa          $ \oa 
  -> pack  (S.plusPtr buf oa)             Word8be (w8 $ ord c) $ \os
  -> packs (S.plusPtr buf (oa + os)) sep  fb       xb          $ \ob
  -> k (oa + os + ob)
 {-# INLINE packs #-}

 

---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}


