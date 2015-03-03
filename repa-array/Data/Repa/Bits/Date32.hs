
module Data.Repa.Bits.Date32
        ( -- * Date conversion
          packDate32,           unpackDate32
        , nextDate32
        , readYYYYsMMsDD)
where
import Data.Repa.Array.Material.Foreign                 as A
import Data.Repa.Array.Material.Unboxed                 as A
import Data.Repa.Array                                  as A
import Data.Word
import Data.Bits
import GHC.Exts
import GHC.Word


-- Dates ----------------------------------------------------------------------
-- | Pack a year, month and day into a `Word32`.
--
--   Bitwise format is:
--   
-- @
-- 32             16       8      0 
-- | year          | month | day  |
-- @
--
--   If they components of the date are out-of-range then they will be bit-wise
--   truncate so they fit in their destination fields.
--  
packDate32   :: (Word, Word, Word) -> Word32
packDate32 (yy, mm, dd) 
        =   ((fromIntegral yy .&. 0x0ffff) `shiftL` 16) 
        .|. ((fromIntegral mm .&. 0x0ff)   `shiftL` 8)
        .|.  (fromIntegral dd .&. 0x0ff)
{-# INLINE packDate32 #-}


-- | Inverse of `packDate32`.
--
--   This function does a simple bit-wise unpacking of the given `Word32`, 
--   and does not guarantee that the returned fields are within a valid 
--   range for the given calendar date.
--
unpackDate32  :: Word32 -> (Word, Word, Word)
unpackDate32 date
        = ( fromIntegral $ (date `shiftR` 16) .&. 0x0ffff
          , fromIntegral $ (date `shiftR` 8)  .&. 0x0ff
          , fromIntegral $ date               .&. 0x0ff)
{-# INLINE unpackDate32 #-}


-- | Yield the next date in the series.
--
--   This assumes leap years occur every four years, 
--   which is valid before after year 1900 and before year 2100.
--
nextDate32  :: Word32 -> Word32
nextDate32 (W32# date)
          = W32# (nextDate32' date)
{-# INLINE nextDate32 #-}

nextDate32' :: Word# -> Word#
nextDate32' !date
 | (yy,  mm, dd) <- unpackDate32 (W32# date)
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
 = case packDate32 (yy', mm', dd') of
        W32# w  -> w
{-# NOINLINE nextDate32' #-}


-- | Read a date in ASCII YYYYsMMsDD format, using the given separator
--   character 's'.
--
--   TODO: avoid going via lists.
--
readYYYYsMMsDD 
        :: BulkI l Char
        => Char -> Array l Char -> Maybe (Word, Word, Word)
readYYYYsMMsDD sep arr
 = case words 
        $ A.toList
        $ A.mapS U (\c -> if c == sep then ' ' else c) arr of
                [yy, mm, dd]    -> Just (read yy, read mm, read dd)
                _               -> Nothing
{-# INLINE readYYYYsMMsDD #-}
