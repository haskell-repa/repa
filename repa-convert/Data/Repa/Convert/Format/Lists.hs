
module Data.Repa.Convert.Format.Lists 
        ( -- * ASCII Strings
          FixCharList (..)
        , VarCharList (..), unpackCharList)
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Format.Binary
import Data.Monoid
import Data.Word
import Data.Char
import GHC.Exts
import qualified Foreign.Storable               as S
import qualified Foreign.Ptr                    as S
import Prelude hiding (fail)
#include "repa-convert.h"


---------------------------------------------------------------------------------------------------
-- | Fixed length list of characters.
--
-- * When serialised, the \"string\" is not escaped, nor does it have
--   surrounding quotes.
--   
-- * When packing, the length of the provided string must match
--   the field width, else packing will fail.
--
-- * When unpacking, the length of the result will be as set
--   by the field width.
--
data FixCharList                = FixCharList Int    deriving (Eq, Show)
instance Format FixCharList where
 type Value (FixCharList)       = String
 fieldCount _                   = 1
 minSize    (FixCharList len)   = len
 fixedSize  (FixCharList len)   = Just len
 packedSize (FixCharList len) _ = Just len
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable FixCharList where
 
  pack (FixCharList len) xs 
   |  length xs == len
   =  Packer $ \buf k
   -> do mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x)) 
                $ zip [0 .. len - 1] xs
         k (S.plusPtr buf len)

   | otherwise
   = Packer $ \_ _ -> return Nothing
  {-# NOINLINE pack #-}

  packer f v 
   = fromPacker (pack f v)
  {-# INLINE packer #-}

  unpacker (FixCharList len@(I# len')) start end _stop fail eat
   = do 
        let lenBuf = I# (minusAddr# end start)
        if  lenBuf < len
         then fail
         else 
          do let load_unpackChar o
                   = do x :: Word8 <- S.peekByteOff (pw8 start) o
                        return $ chr $ fromIntegral x
                 {-# INLINE load_unpackChar #-}

             xs      <- mapM load_unpackChar [0 .. len - 1]
             eat (plusAddr# start len') xs
  {-# INLINE unpacker #-}


---------------------------------------------------------------------------------------------------
-- | Like `FixCharList`, but a variable length string.
data VarCharList = VarCharList  deriving (Eq, Show)
instance Format VarCharList     where
 type Value VarCharList         = String

 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 0
 {-# INLINE minSize    #-}

 fixedSize  VarCharList         = Nothing
 {-# INLINE fixedSize  #-}

 packedSize VarCharList xs      = Just $ length xs
 {-# NOINLINE packedSize #-}


instance Packable VarCharList where

  pack VarCharList xx
   = case xx of
        []       -> mempty
        (x : xs) -> pack Word8be (w8 $ ord x) <> pack VarCharList xs
  {-# NOINLINE pack #-}

  packer f v 
   = fromPacker (pack f v)
  {-# INLINE packer #-}

  unpacker VarCharList start end stop _fail eat
   = do (Ptr ptr, str)      <- unpackCharList (pw8 start) (pw8 end) stop
        eat ptr str
  {-# INLINE unpack #-}


-- | Unpack a ascii text from the given buffer.
unpackCharList
        :: S.Ptr Word8      -- ^ First byte in buffer.
        -> S.Ptr Word8      -- ^ First byte after buffer.
        -> (Word8 -> Bool)  -- ^ Detect field deliminator.
        -> IO (S.Ptr Word8, [Char])

unpackCharList start end stop
 = go start []
 where  go !ptr !acc
         | ptr >= end
         = return (ptr, reverse acc)

         | otherwise
         = do   w :: Word8 <- S.peek ptr
                if stop w 
                 then do
                   return (ptr, reverse acc)
                 else do
                   let !ptr'  = S.plusPtr ptr 1
                   go ptr' ((chr $ fromIntegral w) : acc)
{-# NOINLINE unpackCharList #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}

