{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Convert.Format.Fields
        ( App (..)
        , Sep (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Binary
import Data.Maybe
import Data.Word
import Data.Char
import Control.Monad
import qualified Foreign.Ptr                    as S
import qualified Foreign.Storable               as S


---------------------------------------------------------------------------------------------------
-- | Formatting fields.
instance (Format a, Format b) 
       => Format (a :*: b) where

 type Value (a :*: b)
  = Value a :*: Value b

 minSize    (fa :*: fb)
  = minSize fa + minSize fb
 {-# INLINE minSize #-}

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

 pack   !buf (fa :*: fb) (xa :*: xb) k
  =  pack buf                  fa xa $ \oa 
  -> pack (S.plusPtr buf oa)   fb xb $ \ob
  -> k (oa + ob)
 {-# INLINE pack #-}

 unpack !buf !len (fa :*: fb) k
  =  unpack buf len                       fa $ \(xa, oa)
  -> unpack (S.plusPtr buf oa) (len - oa) fb $ \(xb, ob)
  -> k (xa :*: xb, oa + ob)
 {-# INLINE unpack #-}


-- Field Containers -------------------------------------------------------------------------------
-- | Append fields without separators.
data App f      = App f         deriving Show


-- | Type used internally to carry the separating character.
data CApp       = CApp          deriving Show


-- | Appended fields.
instance Format f 
      => Format (App f) where
 type Value (App f)     = Value      f
 minSize    (App f)     = minSize    f
 fieldCount (App f)     = fieldCount f
 fixedSize  (App f)     = fixedSize  f
 packedSize (App f) x   = packedSize f x
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


-- | Packing appended fields.
instance (Packable fa, Packables CApp fb)
       => Packables CApp (fa :*: fb) where

 packs !buf !sep (fa :*: fb) (xa :*: xb) k
  =  pack  buf                    fa xa $ \oa 
  -> packs (S.plusPtr buf oa) sep fb xb $ \ob
  -> k (oa + ob)
 {-# INLINE packs #-}

 unpacks !buf !len !sep (fa :*: fb) k
  =  unpack  buf                 len           fa  $ \(xa, oa)
  -> unpacks (S.plusPtr buf oa) (len - oa) sep fb  $ \(xb, ob)
  -> k (xa :*: xb, oa + ob)
 {-# INLINE unpacks #-}


-- | Packing appended fields.
instance (Packable f1, Packable f2, Packables CApp f2) 
       => Packable (App (f1 :*: f2)) where

 pack   !buf      (App f) x k     = packs   buf     CApp f x k
 unpack !buf !len (App f)   k     = unpacks buf len CApp f   k
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


-- Separated Fields -------------------------------------------------------------------------------
-- | Separate fields with the given character.
data Sep f      = Sep  Char f   deriving Show


-- | Type used internally to carry the separating character.
data CSep       = CSep Char     deriving Show


-- | Separated fields.
instance Format f
      => Format (Sep f) where
 type Value (Sep f)     = Value f

 minSize    (Sep _ f)
  = let !n      = fromMaybe 0 $ fieldCount f
    in  minSize f   + (if n == 0  then 0 else n - 1)

 fieldCount (Sep _ f)
  = fieldCount f

 fixedSize  (Sep _ f)
  = do  n       <- fieldCount f
        s       <- fixedSize f
        return  $ s + (if n == 0 then 0 else n - 1)

 packedSize (Sep _ f) x   
  = do  n       <- fieldCount f
        s       <- packedSize f x
        return  $ s + (if n == 0 then 0 else n - 1)
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


-- | Packing separated fields.
instance (Packable f1, Packable f2, Packables CSep f2) 
       => Packable (Sep (f1 :*: f2)) where

 pack   !buf      (Sep c f) x k   = packs   buf     (CSep c) f x k
 unpack !buf !len (Sep c f)   k   = unpacks buf len (CSep c) f   k
 {-# INLINE pack   #-}
 {-# INLINE unpack #-}


-- | Packing separated fields.
instance (Packable fa, Packables CSep fb)
       => Packables CSep (fa :*: fb) where

 packs   !buf     sep@(CSep c) (fa :*: fb) (xa :*: xb) k
  =  pack  buf                            fa       xa          $ \oa 
  -> pack  (S.plusPtr buf  oa)            Word8be (w8 $ ord c) $ \os
  -> packs (S.plusPtr buf (oa + os)) sep  fb       xb          $ \ob
  -> k (oa + os + ob)
 {-# INLINE packs #-}

 unpacks !buf !len sep@(CSep c) (fa :*: fb) k
  = findSep (w8 $ ord c) buf len $ \pos
  -> let
        -- The following size code should be evaluated statically via
        -- inlining and GHC simplifications.
        !sa = minSize fa
        !na = fromMaybe 0 (liftM (\x -> x - 1) $ fieldCount fa)
        !sb = minSize fb
        !nb = fromMaybe 0 (liftM (\x -> x - 1) $ fieldCount fb)

     in if   (sa <= pos)
          && (sa + na + 1 + sb + nb <= len) -- Needed when last field is fixed-size.
          then unpack  buf                       pos               fa  $ \(xa, oa)
            -> unpacks (S.plusPtr buf (oa + 1)) (len - oa - 1) sep fb  $ \(xb, ob)
            -> k (xa :*: xb, oa + 1 + ob)
          else return Nothing
 {-# INLINE unpacks #-}


-- | Find the first occurrence of the given separating character in the
--   buffer, or `Nothing` if we don't find it before the buffer ends.
findSep :: Word8                  -- ^ Separating character.
        -> S.Ptr Word8            -- ^ Buffer.
        -> Int                    -- ^ Buffer length
        -> (Int -> IO (Maybe a))  -- ^ Continuation taking separator position.      
        -> IO (Maybe a)

findSep !sep !buf !len k 
 = loop_findSep 0
 where  
        loop_findSep !ix
         | ix >= len    
         = return Nothing

         | otherwise
         = do   x :: Word8  <- S.peekByteOff buf ix
                if x == sep
                 then k ix
                 else loop_findSep (ix + 1)
        {-# INLINE loop_findSep #-}
{-# INLINE findSep #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}


