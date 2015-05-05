
module Data.Repa.Convert.Format.Date32
        ( YYYYsMMsDD (..)
        , DDsMMsYYYY (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Binary
import Data.Repa.Bits.Date32                    (Date32)
import Data.Char
import Data.Word
import qualified Data.Repa.Bits.Date32          as Date32
import qualified Foreign.Ptr                    as S


cw8 :: Char -> Word8
cw8 c = fromIntegral $ ord c
{-# INLINE cw8 #-}


---------------------------------------------------------------------------------------- YYYYsMMsDD
-- | Date32 in ASCII YYYYsMMsDD format.
data YYYYsMMsDD         = YYYYsMMsDD Char       deriving (Eq, Show)
instance Format YYYYsMMsDD where
 type Value YYYYsMMsDD  = Date32
 fieldCount _           = 1
 minSize    _           = 10
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable YYYYsMMsDD where

 unpack buf len (YYYYsMMsDD s) k
  = do  r       <- Date32.loadYYYYsMMsDD (fromIntegral $ ord s) buf len
        case r of
         Just (d, o)    -> k (d, o)
         Nothing        -> return Nothing
 {-# INLINE unpack #-}

 pack   buf  (YYYYsMMsDD s) v k
  | (yy', mm', dd')        <- Date32.unpack v
  , yy  <- fromIntegral yy'
  , mm  <- fromIntegral mm'
  , dd  <- fromIntegral dd'
  =  pack buf                                   (IntAsc0 4) yy      $ \oy  -> 
     pack (S.plusPtr buf oy)                    Word8be     (cw8 s) $ \os1 ->
     pack (S.plusPtr buf (oy + os1))            (IntAsc0 2) mm      $ \om  ->
     pack (S.plusPtr buf (oy + os1 + om))       Word8be     (cw8 s) $ \os2 ->
     pack (S.plusPtr buf (oy + os1 + om + os2)) (IntAsc0 2) dd      $ \od  -> 
     k (oy + os1 + om + os2 + od)
 {-# INLINE pack #-}


---------------------------------------------------------------------------------------- DDsMMsYYYY
-- | Date32 in ASCII DDsMMsYYYY format.
data DDsMMsYYYY         = DDsMMsYYYY Char       deriving (Eq, Show)
instance Format DDsMMsYYYY where
 type Value DDsMMsYYYY  = Date32
 fieldCount _           = 1
 minSize    _           = 10
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable DDsMMsYYYY where

 unpack buf len (DDsMMsYYYY s) k
  = do  r       <- Date32.loadDDsMMsYYYY (fromIntegral $ ord s) buf len
        case r of
         Just (d, o)    -> k (d, o)
         Nothing        -> return Nothing
 {-# INLINE unpack #-}

 pack   buf     (DDsMMsYYYY s) v k
  | (yy', mm', dd')        <- Date32.unpack v
  , yy  <- fromIntegral yy'
  , mm  <- fromIntegral mm'
  , dd  <- fromIntegral dd'
  =  pack buf                                   (IntAsc0 2) dd      $ \od  -> 
     pack (S.plusPtr buf  od)                   Word8be     (cw8 s) $ \os1 ->
     pack (S.plusPtr buf (od + os1))            (IntAsc0 2) mm      $ \om  ->
     pack (S.plusPtr buf (od + os1 + om))       Word8be     (cw8 s) $ \os2 ->
     pack (S.plusPtr buf (od + os1 + om + os2)) (IntAsc0 4) yy      $ \oy  -> 
     k (od + os1 + om + os2 + oy)
 {-# INLINE pack #-}

