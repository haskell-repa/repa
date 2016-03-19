
module Data.Repa.Array.Material.Auto.Operator.Unpackables
        (Unpackables (..))
where

import Data.Repa.Convert.Format
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Auto
import qualified Data.Repa.Array.Generic.Unpacks        as G

import Data.Repa.Convert.Formats

import Data.Word


---------------------------------------------------------------------------------------------------
class Unpackables format where

 -- | Unpack an encoded table of values from an array of bytes,
 --   and write the result into an existing buffer.
 --
 --   The implementation invokes specialised code for each format.
 --
 unpacksToBuffer
        :: format
        -> Word8
        -> Array  F Word8               -- ^ Encoded source data.
        -> Buffer A Int                 -- ^ Starting indices fields in current column.
        -> Buffer A Int                 -- ^ Ending   indices each row.
        -> Buffer A (Value format)      -- ^ Buffer to write the result fields to.
        -> IO (Maybe (Int, Int))


instance Unpackable a => Unpackables (MaybeChars a) where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_MaybeChars          f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables VarChars where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_VarChars            f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables VarCharString where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_VarCharString       f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables VarText where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut 
  = unpacksToBuffer_VarText             f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables VarTextString where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_VarTextString       f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables IntAsc where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_IntAsc              f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables IntAsc0 where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_IntAsc0             f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables DoubleAsc where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_DoubleAsc           f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables DoubleFixedPack where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_DoubleFixedPack     f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables YYYYsMMsDD where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_YYYYsMMsDD          f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables DDsMMsYYYY where
 unpacksToBuffer                        f c arr ixsStart ixsEnd bufOut
  = unpacksToBuffer_DDsMMsYYYY          f c arr ixsStart ixsEnd bufOut
 {-# INLINE unpacksToBuffer #-}


instance Unpackables (Sep ()) where
 unpacksToBuffer                        _f _c _arr _ixsStart _ixsEnd _bufOut
  = return Nothing
 {-# INLINE unpacksToBuffer #-}


instance ( Unpackables f1
         , Unpackables (Sep fs)
         , Value (Sep fs) ~ Value fs)
      => Unpackables (Sep (f1 :*: fs)) where

 unpacksToBuffer
        (SepCons _ fmt1 fmts) 
        c arr ixsStart ixsEnd 
        (ABuffer_Prod bufA bufs)
  = do  mErr      <- unpacksToBuffer fmt1 c arr ixsStart ixsEnd bufA
        case mErr of 
         Nothing  -> unpacksToBuffer fmts c arr ixsStart ixsEnd bufs
         Just err -> return (Just err)
 {-# INLINE unpacksToBuffer #-}
 --  INLINE so that the sequence of calls to 'unpacksToBuffer' will
 --  be unfolded into the client module. The instances themselves are
 --  set to NOINLINE, so this won't cause code explosion.


---------------------------------------------------------------------------------------------------
-- The following instances are hand specialised and set to NOINLINE
-- so that we only generate them once, and in this module. 
-- We don't want to re-derive this code in client programs.
--
-- We define a top level binding for each instance to force specialisation.
-- Using the same code directly in a type class instance doesn't seem
-- to work.
--
type UnpacksColumn format
        =  format
        -> Word8
        -> Array  F Word8
        -> Buffer A Int
        -> Buffer A Int
        -> Buffer A (Value format)
        -> IO (Maybe (Int, Int))

unpacksToBuffer_MaybeChars      :: Unpackable a => UnpacksColumn (MaybeChars a)
unpacksToBuffer_MaybeChars      f@(MaybeChars _ _)      c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_MaybeChars #-}


unpacksToBuffer_VarChars        :: UnpacksColumn VarChars
unpacksToBuffer_VarChars        f@VarChars              c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_VarChars #-}


unpacksToBuffer_VarCharString   :: UnpacksColumn VarCharString
unpacksToBuffer_VarCharString   f@VarCharString         c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_VarCharString #-}


unpacksToBuffer_VarText         :: UnpacksColumn VarText
unpacksToBuffer_VarText         f@VarText               c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_VarText #-}


unpacksToBuffer_VarTextString   :: UnpacksColumn VarTextString
unpacksToBuffer_VarTextString   f@VarTextString         c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_VarTextString #-}


unpacksToBuffer_IntAsc          :: UnpacksColumn IntAsc
unpacksToBuffer_IntAsc          f@IntAsc                c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_IntAsc #-}


unpacksToBuffer_IntAsc0         :: UnpacksColumn IntAsc0
unpacksToBuffer_IntAsc0         f@(IntAsc0 _)           c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_IntAsc0 #-}


unpacksToBuffer_DoubleAsc       :: UnpacksColumn DoubleAsc
unpacksToBuffer_DoubleAsc       f@DoubleAsc             c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_DoubleAsc #-}


unpacksToBuffer_DoubleFixedPack :: UnpacksColumn DoubleFixedPack
unpacksToBuffer_DoubleFixedPack f@(DoubleFixedPack _)   c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_DoubleFixedPack #-}


unpacksToBuffer_YYYYsMMsDD      :: UnpacksColumn YYYYsMMsDD
unpacksToBuffer_YYYYsMMsDD      f@(YYYYsMMsDD _)        c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_YYYYsMMsDD #-}


unpacksToBuffer_DDsMMsYYYY      :: UnpacksColumn DDsMMsYYYY
unpacksToBuffer_DDsMMsYYYY      f@(DDsMMsYYYY _)        c arr ixsStart ixsEnd ixsOut
 = G.unsafeUnpacksToBuffer      f                       c arr ixsStart ixsEnd ixsOut
{-# NOINLINE unpacksToBuffer_DDsMMsYYYY #-}

