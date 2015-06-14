
module Data.Repa.Convert.Format.Date32
        ( YYYYsMMsDD (..)
        , DDsMMsYYYY (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Binary
import Data.Monoid
import Data.Char
import Data.Word
import Data.Repa.Scalar.Date32                  (Date32)
import qualified Data.Repa.Scalar.Date32        as Date32
import qualified Foreign.Ptr                    as F
import Prelude hiding (fail)
#include "repa-convert.h"


---------------------------------------------------------------------------------------- YYYYsMMsDD
-- | Date32 in ASCII YYYYsMMsDD format.
data YYYYsMMsDD         = YYYYsMMsDD Char       deriving (Eq, Show)
instance Format YYYYsMMsDD where
 type Value YYYYsMMsDD  = Date32
 fieldCount _           = 1
 minSize    _           = 10
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable YYYYsMMsDD where

 pack  (YYYYsMMsDD s) v 
  | (yy', mm', dd')        <- Date32.unpack v
  , yy  <- fromIntegral yy'
  , mm  <- fromIntegral mm'
  , dd  <- fromIntegral dd'
  =        pack (IntAsc0 4) yy
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) mm
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) dd
 {-# INLINE_INNER pack #-}

 unpack (YYYYsMMsDD s)
  =  Unpacker $ \start end _stop fail eat
  -> do
        let !len = F.minusPtr end start
        r       <- Date32.loadYYYYsMMsDD (fromIntegral $ ord s) start len
        case r of
         Just (d, o)    -> eat (F.plusPtr start o) d
         Nothing        -> fail
 {-# INLINE_INNER unpack #-}


---------------------------------------------------------------------------------------- DDsMMsYYYY
-- | Date32 in ASCII DDsMMsYYYY format.
data DDsMMsYYYY         = DDsMMsYYYY Char       deriving (Eq, Show)
instance Format DDsMMsYYYY where
 type Value DDsMMsYYYY  = Date32
 fieldCount _           = 1
 minSize    _           = 10
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable DDsMMsYYYY where

 pack   (DDsMMsYYYY s) v
  | (yy', mm', dd')        <- Date32.unpack v
  , yy  <- fromIntegral yy'
  , mm  <- fromIntegral mm'
  , dd  <- fromIntegral dd'
  =        pack (IntAsc0 2) dd
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) mm
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 4) yy
 {-# INLINE_INNER pack #-}

 unpack (DDsMMsYYYY s)
  =  Unpacker $ \start end _stop fail eat
  -> do
        let !len = F.minusPtr end start
        r       <- Date32.loadDDsMMsYYYY (fromIntegral $ ord s) start len
        case r of
         Just (d, o)    -> eat (F.plusPtr start o) d
         Nothing        -> fail
 {-# INLINE_INNER unpack #-}


---------------------------------------------------------------------------------------------------
cw8 :: Char -> Word8
cw8 c = fromIntegral $ ord c
{-# INLINE_INNER cw8 #-}


