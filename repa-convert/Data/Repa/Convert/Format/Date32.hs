
module Data.Repa.Convert.Format.Date32
        ( YYYYsMMsDD (..)
        , DDsMMsYYYY (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Date32
import Data.Repa.Bits.Date32
import Data.Char


---------------------------------------------------------------------------------------- YYYYsMMsDD
-- | Date32 in ASCII YYYYsMMsDD format.
data YYYYsMMsDD         = YYYYsMMsDD Char       deriving (Eq, Show)
instance Format YYYYsMMsDD where
 type Value YYYYsMMsDD  = Date32
 minSize    _           = 10
 fieldCount _           = Just 1
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable YYYYsMMsDD where

 unpack buf len (YYYYsMMsDD s) k
  = do  r       <- loadYYYYsMMsDD (fromIntegral $ ord s) buf len
        case r of
         Just (d, o)    -> k (d, o)
         Nothing        -> return Nothing
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------- DDsMMsYYYY
-- | Date32 in ASCII DDsMMsYYYY format.
data DDsMMsYYYY         = DDsMMsYYYY Char       deriving (Eq, Show)
instance Format DDsMMsYYYY where
 type Value DDsMMsYYYY  = Date32
 minSize    _           = 10
 fieldCount _           = Just 1
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable DDsMMsYYYY where

 unpack buf len (DDsMMsYYYY s) k
  = do  r       <- loadDDsMMsYYYY (fromIntegral $ ord s) buf len
        case r of
         Just (d, o)    -> k (d, o)
         Nothing        -> return Nothing
 {-# INLINE unpack #-}
