
module Data.Repa.Bits.Date32
        ( Date32
        , pack, unpack
        , next)
where
import Data.Word
import Data.Bits
import GHC.Exts
import GHC.Word
import Foreign.Storable
import Foreign.Ptr
import Control.Monad
import Prelude                                          as P


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


