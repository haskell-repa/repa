
module Data.Repa.Convert.Format.Sep
        ( Sep       (..)
        , SepFormat (..)
        , SepMeta   (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Format.Binary
import Data.Repa.Scalar.Product
import Data.Monoid
import Data.Word
import Data.Char
import GHC.Exts
import Prelude hiding (fail)
#include "repa-convert.h"


-- | Separate fields with the given character.
--
--   * The separating character is un-escapable. 
--   * The format @(Sep ',')@ does NOT parse a CSV
--     file according to the CSV specification: http://tools.ietf.org/html/rfc4180.
--
--   * The type is kept abstract as we cache some pre-computed values
--     we use to unpack this format. Use `mkSep` to make one.
--
data Sep f where
        SepNil  :: Sep ()

        SepCons :: {-# UNPACK #-} !SepMeta      -- Meta data about this format.
                -> !f                           -- Format of head field.
                -> Sep fs                       -- Spec for rest of fields.
                -> Sep (f :*: fs)


-- | Precomputed information about this format.
data SepMeta
        = SepMeta
        { -- | Length of this format, in fields.
          smFieldCount          :: !Int

          -- | Minimum length of this format, in bytes.
        , smMinSize             :: !Int

          -- | Fixed size of this format.
        , smFixedSize           :: !(Maybe Int)

          -- | Separating charater for this format.
        , smSepChar             :: !Char }


---------------------------------------------------------------------------------------------------
class SepFormat f where
 mkSep :: Char -> f -> Sep f

instance SepFormat () where
 mkSep _ () = SepNil
 {-# INLINE mkSep #-}


instance (Format f1, SepFormat fs)
      => SepFormat (f1 :*: fs) where

 mkSep c (f1 :*: fs)
  = case mkSep c fs of
        SepNil
         -> SepCons 
                (SepMeta { smFieldCount  = 1
                         , smMinSize     = minSize f1
                         , smFixedSize   = fixedSize f1
                         , smSepChar     = c })
                f1 SepNil

        sep@(SepCons sm _ _)
         -> SepCons
                (SepMeta { smFieldCount  = 1 + smFieldCount sm
                         , smMinSize     = minSize f1 + 1 + smMinSize sm

                         , smFixedSize   
                            = do s1     <- fixedSize f1
                                 ss     <- smFixedSize sm
                                 return $  s1 + 1 + ss

                         , smSepChar     = c })
                f1 sep
 {-# INLINE mkSep #-}


---------------------------------------------------------------------------------------------------
instance Format (Sep ()) where
 type Value (Sep ())    = ()
 fieldCount SepNil      = 0
 minSize    SepNil      = 0
 fixedSize  SepNil      = return 0
 packedSize SepNil _    = return 0
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable (Sep ()) where
 packer   _fmt _val dst _fails k    
  = k dst
 {-# INLINE packer #-}

 unpacker _fmt start _end _stop _fail eat
  = eat start ()
 {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------------------
instance ( Format f1, Format (Sep fs)
         , Value (Sep fs) ~ Value fs)
        => Format (Sep (f1 :*: fs)) where

 type Value (Sep (f1 :*: fs)) 
        = Value f1 :*: Value fs

 fieldCount (SepCons sm _f1 _sfs)
  = smFieldCount sm
 {-# INLINE fieldCount #-}

 minSize    (SepCons sm _f1 _sfs)
  = smMinSize sm
 {-# INLINE minSize #-}

 fixedSize  (SepCons sm _f1 _sfs)
  = smFixedSize sm
 {-# INLINE fixedSize #-}

 packedSize (SepCons _sm f1 sfs) (x1 :*: xs)
  = do  s1       <- packedSize f1  x1
        ss       <- packedSize sfs xs
        let sSep =  zeroOrOne (fieldCount sfs)
        return  $ s1 + sSep + ss 
 {-# INLINE packedSize #-}


---------------------------------------------------------------------------------------------------
instance ( Packable f1
         , Value (Sep ()) ~ Value ())
       => Packable (Sep (f1 :*: ())) where

 packer (SepCons _ f1 _ ) (x1 :*: _) start k
        = packer f1 x1 start k
 {-# INLINE pack #-}

 unpacker (SepCons sm f1 sfs) start end stop fail eat
  = do  let stop' x = w8 (ord (smSepChar sm)) == x || stop x
            {-# INLINE stop' #-}

        unpacker     f1  start    end stop' fail $ \start_x1 x1
         -> unpacker sfs start_x1 end stop  fail $ \start_xs xs
             -> eat start_xs (x1 :*: xs)
 {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------------------
instance ( Packable f1
         , Packable (Sep (f2 :*: fs))
         , Value    (Sep (f2 :*: fs)) ~ Value (f2 :*: fs)
         , Value    (Sep fs)          ~ Value fs)
      => Packable   (Sep (f1 :*: f2 :*: fs)) where

 pack (SepCons sm f1 sfs) (x1 :*: xs)
        =  pack f1  x1 
        <> pack Word8be (w8 $ ord $ smSepChar sm) 
        <> pack sfs xs
 {-# INLINE pack #-}

 packer f v
        = fromPacker $ pack f v
 {-# INLINE packer #-}

 unpacker (SepCons sm f1 sfs) start end stop fail eat
  = do  -- Length of data remaining in the input buffer.
        let len = I# (minusAddr# end start)

        let stop' x = w8 (ord (smSepChar sm)) == x || stop x
            {-# INLINE stop' #-}

        if not (smMinSize sm <= len)
         then fail
         else do
                unpacker     f1  start                   end stop' fail $ \start_x1 x1 
                 -> unpacker sfs (plusAddr# start_x1 1#) end stop  fail $ \start_xs xs
                     -> eat start_xs (x1 :*: xs)
 {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}


-- | Branchless equality used to avoid compile-time explosion in size of core code.
zeroOrOne :: Int -> Int
zeroOrOne (I# i) = I# (1# -# (0# ==# i))
{-# INLINE zeroOrOne #-}

