
-- | Conversions for "Data.Maybe" wrapped formats.
module Data.Repa.Convert.Format.Maybe
        ( MaybeChars    (..)
        , MaybeBytes    (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Format.Bytes
import Data.Word
import GHC.Exts
import Prelude                                  hiding (fail)
import Data.ByteString                          (ByteString)
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Internal       as BS
import qualified Foreign.Storable               as F
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Ptr                    as F
#include "repa-convert.h"


---------------------------------------------------------------------------------------- MaybeChars
-- | Maybe a raw list of characters, or something else.
data MaybeChars f            = MaybeChars String f      deriving (Eq, Show)

instance Format f => Format (MaybeChars f) where
 type Value (MaybeChars f)   
        = Maybe (Value f)

 fieldCount _
        = 1
 {-# INLINE fieldCount #-}

 minSize       (MaybeChars str f) 
  = minSize    (MaybeBytes (BS.pack str) f)

 {-# INLINE minSize    #-}

 fixedSize     (MaybeChars str f)
  = fixedSize  (MaybeBytes (BS.pack str) f)
 {-# INLINE fixedSize #-}

 packedSize    (MaybeChars str f) 
  = kk
  where !bs = BS.pack str
        kk mv
         = packedSize (MaybeBytes bs f) mv
        {-# INLINE kk #-}
 {-# INLINE packedSize #-}


instance Packable f
      => Packable (MaybeChars f) where

 -- Convert the Nothing string to a ByteString which has a better runtime representation.
 -- We do this before accepting the actual value, so the conversion happens only
 -- once, instead of when we pack every value.
 packer    (MaybeChars str f)
  = kk
  where !bs = BS.pack str
        kk x start k
         = packer  (MaybeBytes bs f) x start k
        {-# INLINE kk #-}
 {-# INLINE packer #-}

 -- As above, convert the Nothing string to a ByteString which has a better runtime
 -- representation.
 unpacker  (MaybeChars str f)
  = kk
  where !bs = BS.pack str
        kk start end stop fail eat
         = unpacker (MaybeBytes bs f) start end stop fail eat
        {-# INLINE kk #-}
 {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------- MaybeBytes
-- | Maybe a raw sequence of bytes, or something else.
data MaybeBytes f           = MaybeBytes ByteString f   deriving (Eq, Show)

instance Format f => Format (MaybeBytes f) where

 type Value (MaybeBytes f)   
        = Maybe (Value f)

 fieldCount _
        = 1
 {-# INLINE fieldCount #-}

 minSize    (MaybeBytes str f) 
  = let !(I# ms)   = minSize f
    in  I# (minSize_MaybeBytes str ms)
 {-# INLINE minSize    #-}

 fixedSize  (MaybeBytes str f)
  = fixedSize_MaybeBytes str (fixedSize f) 
 {-# INLINE fixedSize #-}

 packedSize (MaybeBytes str f) mv
  = case mv of
        Nothing -> Just $ BS.length str
        Just v  -> packedSize f v
 {-# NOINLINE packedSize #-}
 --  NOINLINE to hide the case from the simplifier.


-- Minsize, hiding the case expression from the simplifier.
minSize_MaybeBytes   :: ByteString -> Int# -> Int#
minSize_MaybeBytes s i
 = case min (BS.length s) (I# i) of
        I# i' -> i'
{-# NOINLINE minSize_MaybeBytes #-}


-- Fixedsize, hiding the case expression from the simplifier.
fixedSize_MaybeBytes :: ByteString -> Maybe Int -> Maybe Int
fixedSize_MaybeBytes s r
 = case r of
        Nothing -> Nothing
        Just sf -> if BS.length s == sf 
                        then Just sf
                        else Nothing
{-# NOINLINE fixedSize_MaybeBytes #-}
--  NOINLINE to hide the case from the simplifier.


instance Packable f
      => Packable (MaybeBytes f) where

 packer   (MaybeBytes str f) mv start k
  = case mv of
        Nothing -> packer VarBytes str start k
        Just v  -> packer f        v   start k
 {-# NOINLINE pack #-}
  -- TODO we're NOINLINEing this so we don't duplicate the code for the continuation.
  --      It would be better to use an Either format and use that to express the branch.

 unpacker (MaybeBytes (BS.PS bsFptr bsStart bsLen) f) 
          start end stop fail eat
  = F.withForeignPtr bsFptr
  $ \bsPtr_
  -> let
        -- Length of the input buffer.
        !lenBuf = F.minusPtr (pw8 end) (pw8 start)

        -- Pointer to active bytes in Nothing string.
        !bsPtr  = F.plusPtr bsPtr_ bsStart

        -- Check for the Nothing string,
        --   We do an early exit, bailing out on the first byte that doesn't match.
        --   If this isn't the Nothing string then we need to unpack the inner format.
        checkNothing !ix

         -- Matched the complete Nothing string.
         | ix >= bsLen
         = do   -- Give the continuation the starting pointer for the next field.
                let !(Ptr start') = F.plusPtr (pw8 start) ix
                eatIt start' Nothing

         -- Hit the end of the buffer before matching the Nothing string.
         | ix >= lenBuf     
         = unpackInner

         -- Check if the next byte is the next byte in the Nothing string.
         | otherwise
         = do  !x  <- F.peekByteOff (pw8 start) ix
               if stop x 
                then unpackInner
                else do
                        !x'  <- F.peekByteOff bsPtr ix
                        if x /= x'
                         then unpackInner
                         else checkNothing (ix + 1)

        unpackInner 
         = unpacker f start end stop fail 
         $ \addr x -> eatIt addr (Just x)
        {-# NOINLINE unpackInner #-}

        eatIt addr val
         = eat addr val
        {-# NOINLINE eatIt #-}
        --  NOINLINE so we don't duplicate the continuation.

     in checkNothing 0
 {-# INLINE unpacker #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
