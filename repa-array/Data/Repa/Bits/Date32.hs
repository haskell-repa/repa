{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Bits.Date32
        ( Date32
        , pack, unpack
        , next
        , range
        , pretty
        , readYYYYsMMsDD
        , readDDsMMsYYYY)
where
import Data.Repa.Array                  
import Data.Repa.Array.Auto.Convert
import qualified Data.Repa.Array.Generic.Target         as A
import qualified Data.Repa.Array.Generic.Index          as A
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Data.Repa.Array.Generic                as A
import qualified Data.Repa.Array.Meta.Window            as A
import qualified Data.Repa.Fusion.Unpack                as A
import Data.Word
import Data.Bits
import GHC.Exts
import GHC.Word
import Foreign.Storable
import Foreign.Ptr
import Control.Monad
import Prelude                                          as P
#include "repa-array.h"

-- | A date packed into a 32-bit word.
--
--   The bitwise format is:
--
--   @
--   32             16       8      0 
--   | year          | month | day  |
--   @
--
--   Pros: Packing and unpacking a Date32 is simpler than using other formats
--   that represent dates as a number of days from some epoch. We can also
--   avoid worrying about what the epoch should be, and the representation
--   will not overflow until year 65536. 
--
--   Cons: Computing a range of dates is slower than with representations
--   using an epoch, as we cannot simply add one to get to the next valid date.
--
newtype Date32 
        = Date32 Word32
        deriving (Eq, Ord, Show)


instance Storable Date32 where
 sizeOf (Date32 w)      = sizeOf w
 alignment (Date32 w)   = alignment w
 peek ptr               = liftM Date32 (peek (castPtr ptr))
 poke ptr (Date32 w)    = poke (castPtr ptr) w
 {-# INLINE sizeOf    #-}
 {-# INLINE alignment #-}
 {-# INLINE peek      #-}
 {-# INLINE poke      #-}


instance A.Bulk A.A Date32 where
 data Array A.A Date32           = AArray_Date32 !(A.Array A.F Date32)
 layout (AArray_Date32 arr)      = A.Auto (A.length arr)
 index  (AArray_Date32 arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (A.Array A.A Date32)


instance A.Windowable A.A Date32 where
 window st len (AArray_Date32 arr) 
  = AArray_Date32 (A.window st len arr)
 {-# INLINE_ARRAY window #-}


instance A.Target A.A Date32 where
 data Buffer A.A Date32            
  = ABuffer_Date32 !(A.Buffer A.F Date32)

 unsafeNewBuffer    (A.Auto len)     
  = liftM ABuffer_Date32 $ A.unsafeNewBuffer (A.Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Date32 arr) ix
  = A.unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Date32 arr) ix x
  = A.unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Date32 arr) bump
  = liftM ABuffer_Date32 $ A.unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Date32 arr)
  = liftM AArray_Date32  $ A.unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Date32 arr)
  = liftM ABuffer_Date32 $ A.unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Date32 buf)
  = liftM ABuffer_Date32 $ A.unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer  (ABuffer_Date32 buf)
  = A.touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Date32 buf)
  = A.Auto $ A.extent $ A.bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (A.Unpack (A.Buffer A.F Date32)) t 
      => (A.Unpack (A.Buffer A.A Date32)) t where
 unpack (ABuffer_Date32 buf)   = A.unpack buf
 repack (ABuffer_Date32 x) buf = ABuffer_Date32 (A.repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


---------------------------------------------------------------------------------------------------
-- | Pack a year, month and day into a `Word32`. 
--
--   If any components of the date are out-of-range then they will be bit-wise
--   truncated so they fit in their destination fields.
--
pack   :: (Word, Word, Word) -> Date32
pack (yy, mm, dd) 
        = Date32
        $   ((fromIntegral yy .&. 0x0ffff) `shiftL` 16) 
        .|. ((fromIntegral mm .&. 0x0ff)   `shiftL` 8)
        .|.  (fromIntegral dd .&. 0x0ff)
{-# INLINE pack #-}


-- | Inverse of `pack`.
--
--   This function does a simple bit-wise unpacking of the given `Word32`, 
--   and does not guarantee that the returned fields are within a valid 
--   range for the given calendar date.
--
unpack  :: Date32 -> (Word, Word, Word)
unpack (Date32 date)
        = ( fromIntegral $ (date `shiftR` 16) .&. 0x0ffff
          , fromIntegral $ (date `shiftR` 8)  .&. 0x0ff
          , fromIntegral $ date               .&. 0x0ff)
{-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Yield the next date in the series.
--
--   This assumes leap years occur every four years, 
--   which is valid after year 1900 and before year 2100.
--
next  :: Date32 -> Date32
next (Date32 (W32# date))
          = Date32 (W32# (next' date))
{-# INLINE next #-}

next' :: Word# -> Word#
next' !date
 | (yy,  mm, dd) <- unpack (Date32 (W32# date))
 , (yy', mm', dd') 
     <- case mm of
        1       -> if dd >= 31  then (yy,     2, 1) else (yy, mm, dd + 1)  -- Jan

        2       -> if yy `mod` 4 == 0                                      -- Feb
                        then if dd >= 29
                                then (yy,     3,      1) 
                                else (yy,    mm, dd + 1)
                        else if dd >= 28
                                then (yy,     3,      1)
                                else (yy,    mm, dd + 1)

        3       -> if dd >= 31 then (yy,     4, 1) else (yy, mm, dd + 1)  -- Mar
        4       -> if dd >= 30 then (yy,     5, 1) else (yy, mm, dd + 1)  -- Apr
        5       -> if dd >= 31 then (yy,     6, 1) else (yy, mm, dd + 1)  -- May
        6       -> if dd >= 30 then (yy,     7, 1) else (yy, mm, dd + 1)  -- Jun
        7       -> if dd >= 31 then (yy,     8, 1) else (yy, mm, dd + 1)  -- Jul
        8       -> if dd >= 31 then (yy,     9, 1) else (yy, mm, dd + 1)  -- Aug
        9       -> if dd >= 30 then (yy,    10, 1) else (yy, mm, dd + 1)  -- Sep
        10      -> if dd >= 31 then (yy,    11, 1) else (yy, mm, dd + 1)  -- Oct
        11      -> if dd >= 30 then (yy,    12, 1) else (yy, mm, dd + 1)  -- Nov
        12      -> if dd >= 31 then (yy + 1, 1, 1) else (yy, mm, dd + 1)  -- Dec
        _       -> (0, 0, 0)
 = case pack (yy', mm', dd') of
        Date32 (W32# w)  -> w
{-# NOINLINE next' #-}


-- | Yield an array containing a range of dates, inclusive of the end points.
---
--   TODO: avoid going via lists.
--
range   :: Date32 -> Date32 -> Array Date32
range from to 
 | to < from    = A.fromList A.A []
 | otherwise    = A.fromList A.A $ go [] from
 where
        go !acc !d   
                | d > to        = P.reverse acc
                | otherwise     = go (d : acc) (next d)
{-# NOINLINE range #-}


---------------------------------------------------------------------------------------------------
-- | Pretty print a `Date32`
---
--  TODO: avoid going via lists.
--
pretty  :: Char         -- ^ Separator for components.
        -> Date32       -- ^ Date to pretty print.
        -> Array Char

pretty !cSep !date
 = let  (yy, mm, dd)    = unpack date
        yy'             = show yy
        mm'             = if mm < 10 then "0" ++ show mm else show mm
        dd'             = if dd < 10 then "0" ++ show dd else show dd
   in   A.fromList A.A $ P.concat [yy', [cSep], mm', [cSep], dd']


---------------------------------------------------------------------------------------------------
-- | Read a `Date32` in ASCII YYYYsMMsDD format, 
--   using the given separator character 's'.
readYYYYsMMsDD :: Char -> Array Char -> Maybe Date32
readYYYYsMMsDD !c !arr
 | I# len               <- A.length arr

 -- year part
 , (# 1#, yy, ix1 #)    <- readIntFromOffset# arr 0#

 -- month part
 , 1# <- ix1 <# len
 , arr `index` (I# ix1) == c
 , (# 1#, mm, ix2 #)    <- readIntFromOffset# arr (ix1 +# 1#)

 -- day part
 , 1# <- ix2 <# len
 , arr `index` (I# ix2) == c
 , (# 1#, dd, _   #)    <- readIntFromOffset# arr (ix2 +# 1#)

 = Just (pack   ( fromIntegral (I# yy)
                , fromIntegral (I# mm)
                , fromIntegral (I# dd)))

 | otherwise
 = Nothing
{-# INLINE [0] readYYYYsMMsDD #-}


---------------------------------------------------------------------------------------------------
-- | Read a `Date32` in ASCII DDsMMsYYYY format, 
--   using the given separator character 's'.
readDDsMMsYYYY :: Char -> Array Char -> Maybe Date32
readDDsMMsYYYY !c !arr
 | I# len               <- A.length arr

 -- day part
 , (# 1#, dd, ix1 #)    <- readIntFromOffset# arr 0#

 -- month part
 , 1# <- ix1 <# len
 , arr `index` (I# ix1) == c
 , (# 1#, mm, ix2 #)    <- readIntFromOffset# arr (ix1 +# 1#)

 -- year part
 , 1# <- ix2 <# len
 , arr `index` (I# ix2) == c
 , (# 1#, yy, _   #)    <- readIntFromOffset# arr (ix2 +# 1#)

 = Just (pack   ( fromIntegral (I# yy)
                , fromIntegral (I# mm)
                , fromIntegral (I# dd)))

 | otherwise
 = Nothing
{-# INLINE [0] readDDsMMsYYYY #-}

