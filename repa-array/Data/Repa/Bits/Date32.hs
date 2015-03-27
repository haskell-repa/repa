
module Data.Repa.Bits.Date32
        ( Date32
        , pack, unpack
        , next
        , range
        , pretty
        , readYYYYsMMsDD
        , readDDsMMsYYYY)
where
import Data.Repa.IO.Convert                             as A
import Data.Repa.Array                                  as A
import Data.Repa.Eval.Array                             as A
import Data.Word
import Data.Bits
import GHC.Exts
import GHC.Word
import Prelude                                          as P
import Data.Repa.Array.Material.Unboxed
import Data.Repa.Array.Material.Foreign


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
range   :: TargetI l Date32
        => Name l -> Date32 -> Date32 -> Array l Date32

range n from to 
 | to < from    = A.fromList n []
 | otherwise    = A.fromList n $ go [] from
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
pretty  :: TargetI l Char
        => Name l       -- ^ Result layout.
        -> Char         -- ^ Separator for components.
        -> Date32       -- ^ Date to pretty print.
        -> Array l Char

pretty !nDst !cSep !date
 = let  (yy, mm, dd)    = unpack date
        yy'             = show yy
        mm'             = if mm < 10 then "0" ++ show mm else show mm
        dd'             = if dd < 10 then "0" ++ show dd else show dd

   in   A.fromList nDst $ P.concat [yy', [cSep], mm', [cSep], dd']


---------------------------------------------------------------------------------------------------
-- | Read a `Date32` in ASCII YYYYsMMsDD format, 
--   using the given separator character 's'.
readYYYYsMMsDD 
        :: BulkI l Char
        => Char -> Array l Char -> Maybe Date32

readYYYYsMMsDD sep arr
 = case words 
        $ A.toList
        $ A.mapS U (\c -> if c == sep then ' ' else c) arr of
                [yy, mm, dd]    -> Just $ pack (read yy, read mm, read dd)
                _               -> Nothing
{-# INLINE [0] readYYYYsMMsDD #-}


-- | Hand specialised version for reading out of foreign memory.
readYYYYsMMsDD_foreign :: Char -> Array F Char -> Maybe Date32
readYYYYsMMsDD_foreign !c !arr
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
{-# INLINE [0] readYYYYsMMsDD_foreign #-}


{-# RULES "readYYYYsMMsDD_foreign" 
    forall c (arr :: Array F Char) 
    . readYYYYsMMsDD c arr = readYYYYsMMsDD_foreign c arr 
 #-}


---------------------------------------------------------------------------------------------------
-- | Read a `Date32` in ASCII DDsMMsYYYY format, 
--   using the given separator character 's'.
readDDsMMsYYYY
        :: BulkI l Char
        => Char -> Array l Char -> Maybe Date32

readDDsMMsYYYY sep arr
 = case words 
        $ A.toList
        $ A.mapS U (\c -> if c == sep then ' ' else c) arr of
                [dd, mm, yy]    -> Just $ pack (read yy, read mm, read dd)
                _               -> Nothing
{-# INLINE [0] readDDsMMsYYYY #-}


-- | Hand specialised version for reading out of foreign memory.
readDDsMMsYYYY_foreign :: Char -> Array F Char -> Maybe Date32
readDDsMMsYYYY_foreign !c !arr
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
{-# INLINE [0] readDDsMMsYYYY_foreign #-}


{-# RULES "readDDsMMsYYYY_foreign" 
    forall c (arr :: Array F Char) 
    . readDDsMMsYYYY c arr = readDDsMMsYYYY_foreign c arr 
 #-}


