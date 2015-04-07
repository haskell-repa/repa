
module Data.Repa.Convert.Date32
        ( loadYYYYsMMsDD
        , loadDDsMMsYYYY)
where
import Data.Repa.Convert.Numeric
import Data.Repa.Bits.Date32
import Data.Word
import GHC.Exts
import qualified Foreign.Ptr            as F
import qualified Foreign.Storable       as F


---------------------------------------------------------------------------------------------------
-- | Read a date in YYYYsMMsDD format from the given buffer.
loadYYYYsMMsDD 
        :: Word8                        -- ^ Separating character.
        -> Ptr Word8                    -- ^ Buffer.
        -> Int                          -- ^ Length of buffer.
        -> IO (Maybe (Date32, Int))     -- ^ Result.

loadYYYYsMMsDD !sep !buf (I# len_)
 = year
 where  year 
         | (# 1#, yy, ix' #)     <- loadInt# buf len_
         = sep1 ix' yy
         | otherwise    = return Nothing

        sep1 ix yy
         |  1# <- ix <# len_    
         =  F.peekByteOff buf (I# ix) >>= \(r :: Word8)
         -> if r == sep
                then month (ix +# 1#) yy 
                else return Nothing

         | otherwise    = return Nothing

        month ix yy
         |  1# <- ix <# len_    
         , (# 1#, mm, o #)    <- loadInt# (buf `F.plusPtr` (I# ix)) len_
         = sep2 (ix +# o) yy mm
         | otherwise    = return Nothing

        sep2 ix yy mm
         |  1#  <- ix <# len_
         =  F.peekByteOff buf (I# ix) >>= \(r :: Word8)
         -> if r == sep
                then day (ix +# 1#) yy mm 
                else return Nothing

         | otherwise    = return Nothing

        day ix yy mm
         | 1# <- ix <# len_
         , (# 1#, dd, o #)    <- loadInt# (buf `F.plusPtr` (I# ix)) len_
         = return 
         $ Just (pack   ( fromIntegral (I# yy)
                        , fromIntegral (I# mm)
                        , fromIntegral (I# dd))
                , I# (ix +# o))

         | otherwise    = return Nothing
{-# INLINE loadYYYYsMMsDD #-}


---------------------------------------------------------------------------------------------------
-- | Read a date in YYYYsMMsDD format from the given buffer.
loadDDsMMsYYYY
        :: Word8                        -- ^ Separating character.
        -> Ptr Word8                    -- ^ Buffer.
        -> Int                          -- ^ Length of buffer.
        -> IO (Maybe (Date32, Int))     -- ^ Result.

loadDDsMMsYYYY !sep !buf (I# len_)
 = day
 where  day 
         | (# 1#, dd, ix' #)     <- loadInt# buf len_
         = sep1 ix' dd
         | otherwise    = return Nothing

        sep1 ix dd
         | 1# <- ix <# len_    
         =  F.peekByteOff buf (I# ix) >>= \(r :: Word8)
         -> if r == sep
                then month (ix +# 1#) dd
                else return Nothing

         | otherwise    = return Nothing

        month ix dd
         | 1# <- ix <# len_    
         , (# 1#, mm, o #)    <- loadInt# (buf `F.plusPtr` (I# ix)) len_
         = sep2 (ix +# o) dd mm
         | otherwise    = return Nothing

        sep2 ix dd mm
         |  1#  <- ix <# len_
         =  F.peekByteOff buf (I# ix) >>= \(r :: Word8)
         -> if r == sep
                then year (ix +# 1#) dd mm
                else return Nothing

         | otherwise    = return Nothing

        year ix dd mm 
         | 1# <- ix <# len_
         , (# 1#, yy, o #)    <- loadInt# (buf `F.plusPtr` (I# ix)) len_
         = return 
         $ Just (pack   ( fromIntegral (I# yy)
                        , fromIntegral (I# mm)
                        , fromIntegral (I# dd))
                , I# (ix +# o))

         | otherwise    = return Nothing
{-# INLINE loadDDsMMsYYYY #-}

