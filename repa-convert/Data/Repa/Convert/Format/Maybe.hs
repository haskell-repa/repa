
-- | Maybe -like formats.
module Data.Repa.Convert.Format.Maybe
        ( MaybeChars    (..)
        , MaybeBytes    (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Bytes
import Data.Char
import Data.Word
import Data.IORef
import GHC.Exts
import Prelude hiding (fail)

import qualified Foreign.Storable       as S
import qualified Foreign.Ptr            as S
import Data.Vector.Unboxed              (Vector)
import qualified Data.Vector.Unboxed    as U


#include "repa-convert.h"


------------------------------------------------------------------------------------- MaybeCharList
-- | Maybe a raw list of characters, or something else.
data MaybeChars f            = MaybeChars String f

instance Format f => Format (MaybeChars f) where
 type Value (MaybeChars f)   
        = Maybe (Value f)

 fieldCount _
        = 1
 {-# INLINE fieldCount #-}

 minSize       (MaybeChars str f) 
  = minSize    (MaybeBytes (vecOfString str) f)

 {-# INLINE minSize    #-}

 fixedSize     (MaybeChars str f)
  = fixedSize  (MaybeBytes (vecOfString str) f)
 {-# INLINE fixedSize #-}

 packedSize    (MaybeChars str f) 
  = kk
  where !vec     = vecOfString str

        kk mv
         = packedSize (MaybeBytes vec f) mv
        {-# INLINE kk #-}
 {-# INLINE packedSize #-}


instance Packable f
      => Packable (MaybeChars f) where

 -- Convert the string to a Vector Word8 which has a better runtime representation.
 -- We do this before accepting the actual value, so the conversion happens only
 -- once, instead of when we pack every value.
 packer    (MaybeChars str f)
  = kk
  where !vec     = vecOfString str

        kk x start k
         = packer  (MaybeBytes vec f) x start k
        {-# INLINE kk #-}
 {-# INLINE packer #-}

 -- As avove, convert the string to a Vector Word8 which has a better runtime
 -- representation.
 unpacker  (MaybeChars str f)
  = kk
  where !vec     = vecOfString str

        kk start end stop fail eat
         = unpacker (MaybeBytes vec f) start end stop fail eat
        {-# INLINE kk #-}
 {-# INLINE unpacker #-}


vecOfString :: String -> Vector Word8
vecOfString str
        = U.map (fromIntegral . ord) 
        $ U.fromList str
{-# NOINLINE vecOfString #-}


------------------------------------------------------------------------------------ MaybeBytes
-- | Maybe a raw sequence of bytes, or something else.
data MaybeBytes f           = MaybeBytes (Vector Word8) f

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
        Nothing -> Just $ U.length str
        Just v  -> packedSize f v
 {-# NOINLINE packedSize #-}


minSize_MaybeBytes   :: Vector Word8 -> Int# -> Int#
minSize_MaybeBytes s i
 = case min (U.length s) (I# i) of
        I# i' -> i'
{-# NOINLINE minSize_MaybeBytes #-}


fixedSize_MaybeBytes :: Vector Word8 -> Maybe Int -> Maybe Int
fixedSize_MaybeBytes s r
 = case r of
        Nothing -> Nothing
        Just sf -> if U.length s == sf 
                        then Just sf
                        else Nothing
{-# NOINLINE fixedSize_MaybeBytes #-}


instance Packable f
      => Packable (MaybeBytes f) where

 packer   (MaybeBytes str f) mv start k
  = case mv of
        Nothing -> packer VarBytes str start k
        Just v  -> packer f        v   start k
 {-# NOINLINE pack #-}
  -- TODO we're need to NOINLINEing this so we don't duplicate the code for the continuation.
  --      It would be better to use an Either format and use that to express the branch.

 unpacker (MaybeBytes str f) start end stop fail eat
  = do  
        -- Length of the input buffer.
        let !lenBuf     = S.minusPtr (pw8 end) (pw8 start)

        -- Length of the Nothing string.
        let !lenMatch   = U.length str

        -- Check for the Nothing string,
        -- if this is not it then unpack the inner format.
        let checkNothing ix

             -- Matched the complete Nothing string.
             | ix >= lenMatch   
             = do
                let !(Ptr start')     = S.plusPtr (pw8 start) ix
                eatIt start' Nothing

             -- Hit the end of the buffer before matching the Nothing string.
             | ix >= lenBuf     
             =  unpackInner

             -- Check if the next byte is the next byte in the Nothing string.
             | otherwise
             = do  x    <- S.peekByteOff (pw8 start) ix
                   if stop x 
                    then unpackInner
                    else if U.unsafeIndex str ix /= x
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

        checkNothing 0
 {-# INLINE unpacker #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
