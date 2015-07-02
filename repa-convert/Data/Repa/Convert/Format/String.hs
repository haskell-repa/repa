
module Data.Repa.Convert.Format.String
        ( -- * ASCII Strings
          VarString     (..))
where
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Word
import Data.Char
import GHC.Exts
import qualified Foreign.Storable               as S
import qualified Foreign.Ptr                    as S
import Prelude hiding (fail)
#include "repa-convert.h"


---------------------------------------------------------------------------------------------------
-- | Variable length string in double quotes, 
--   and standard backslash encoding of special characters.
data VarString = VarString      deriving (Eq, Show)
instance Format VarString       where
 type Value VarString           = String
 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 2
 {-# INLINE minSize #-}

 fixedSize  _                   = Nothing
 {-# INLINE fixedSize #-}
 
 packedSize VarString xs        
  = Just $ length $ show xs     
 {-# NOINLINE packedSize #-}


instance Packable VarString where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 packer     VarString xx          start k
  =  packer VarCharList (show xx) start k
 {-# INLINE pack #-}

 unpacker   VarString start end _stop  fail eat
  = unpackString (pw8 start) (pw8 end) fail eat
 {-# INLINE unpacker #-}


-- | Unpack a string from the given buffer.
unpackString 
        :: S.Ptr Word8                  -- ^ First byte in buffer.
        -> S.Ptr Word8                  -- ^ First byte after buffer.
        -> IO ()                        -- ^ Signal failure.
        -> (Addr# -> [Char] -> IO ())   -- ^ Eat an unpacked value.
        -> IO ()

unpackString start end fail eat
 = open start
 where
        -- Accept the open quotes.
        open !ptr
         | ptr >= end
         = fail

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let !ptr'   =  S.plusPtr ptr 1 
                case chr $ fromIntegral w of
                 '"'    -> go_body ptr' []
                 _      -> fail

        -- Handle the next character in the string.
        go_body !ptr@(Ptr addr) !acc
         | ptr >= end 
         = eat addr (reverse acc)

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let !ptr'@(Ptr addr')   =  S.plusPtr ptr 1
                case chr $ fromIntegral w of
                 '"'    -> eat addr' (reverse acc)
                 '\\'   -> go_escape ptr' acc
                 c      -> go_body   ptr' (c : acc)

        -- Handle escaped character.
        -- The previous character was a '\\'
        go_escape !ptr !acc
         | ptr >= end
         = fail

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let ptr'    =  S.plusPtr ptr 1
                case chr $ fromIntegral w of
                 'a'    -> go_body ptr' ('\a' : acc)
                 'b'    -> go_body ptr' ('\b' : acc)
                 'f'    -> go_body ptr' ('\f' : acc)
                 'n'    -> go_body ptr' ('\n' : acc)
                 'r'    -> go_body ptr' ('\r' : acc)
                 't'    -> go_body ptr' ('\t' : acc)
                 'v'    -> go_body ptr' ('\v' : acc)
                 '\\'   -> go_body ptr' ('\\' : acc)
                 '"'    -> go_body ptr' ('"'  : acc)
                 _      -> fail
{-# NOINLINE unpackString #-}


---------------------------------------------------------------------------------------------------
pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
