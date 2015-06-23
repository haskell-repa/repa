
module Data.Repa.Convert.Format.Lists 
        ( -- * ASCII Strings
          FixAsc (..)
        , VarAsc (..), unpackAsc
        , VarString (..))
where
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Data.Monoid
import Data.Word
import Data.Char
import qualified Foreign.Storable               as S
import qualified Foreign.Ptr                    as S
import Prelude hiding (fail)
#include "repa-convert.h"


---------------------------------------------------------------------------------------------------
-- | Fixed length string.
--   
-- * When packing, the length of the provided string must match
--   the field width, else packing will fail.
--
-- * When unpacking, the length of the result will be as set
--   by the field width.
--
data FixAsc     = FixAsc Int    deriving (Eq, Show)
instance Format FixAsc where
 type Value (FixAsc)            = String
 fieldCount _                   = 1
 minSize    (FixAsc len)        = len
 fixedSize  (FixAsc len)        = Just len
 packedSize (FixAsc len) _      = Just len
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable FixAsc where
 
  pack (FixAsc len) xs 
   |  length xs == len
   =  Packer $ \buf k
   -> do mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x)) 
                $ zip [0 .. len - 1] xs
         k (S.plusPtr buf len)

   | otherwise
   = Packer $ \_ _ -> return Nothing
  {-# NOINLINE pack #-}

  unpack (FixAsc len)
   =  Unpacker $ \start end _stop fail eat
   -> do 
        let lenBuf = S.minusPtr end start
        if  lenBuf < len
         then fail
         else 
          do let load_unpackChar o
                   = do x :: Word8 <- S.peekByteOff start o
                        return $ chr $ fromIntegral x
                 {-# INLINE load_unpackChar #-}

             xs      <- mapM load_unpackChar [0 .. len - 1]
             eat (S.plusPtr start len) xs
  {-# NOINLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Variable length raw string (with no quotes).
data VarAsc = VarAsc            deriving (Eq, Show)
instance Format (VarAsc)        where
 type Value VarAsc              = String
 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 0
 {-# INLINE minSize    #-}

 fixedSize  VarAsc              = Nothing
 {-# INLINE fixedSize  #-}

 packedSize VarAsc xs           = Just $ length xs
 {-# NOINLINE packedSize #-}


instance Packable VarAsc where

  pack VarAsc xx
   = case xx of
        []       -> mempty
        (x : xs) -> pack Word8be (w8 $ ord x) <> pack VarAsc xs
  {-# NOINLINE pack #-}

  unpack VarAsc 
   = Unpacker $ \start end stop _fail eat
   -> do (ptr, str)      <- unpackAsc start end stop
         eat ptr str
  {-# NOINLINE unpack #-}


-- | Unpack a ascii text from the given buffer.
unpackAsc
        :: S.Ptr Word8      -- ^ First byte in buffer.
        -> S.Ptr Word8      -- ^ First byte after buffer.
        -> (Word8 -> Bool)  -- ^ Detect field deliminator.
        -> IO (S.Ptr Word8, [Char])

unpackAsc start end stop
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
{-# NOINLINE unpackAsc #-}


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
 pack VarString xx
  =  pack VarAsc (show xx)
 {-# NOINLINE pack #-}

 unpack VarString
  =  Unpacker $ \start end _stop fail eat
  -> do r       <- unpackString start end
        case r of
         Nothing            -> fail
         Just (start', str) -> eat start' str
 {-# NOINLINE unpack #-}


-- | Unpack a string from the given buffer.
unpackString 
        :: S.Ptr Word8    -- ^ First byte in buffer.
        -> S.Ptr Word8    -- ^ First byte after buffer.
        -> IO (Maybe (S.Ptr Word8, [Char]))

unpackString start end
 = open start
 where
        -- Accept the open quotes.
        open !ptr
         | ptr >= end
         = return $ Nothing

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let !ptr'   =  S.plusPtr ptr 1 
                case chr $ fromIntegral w of
                 '"'    -> go_body ptr' []
                 _      -> return Nothing

        -- Handle the next character in the string.
        go_body !ptr !acc
         | ptr >= end 
         = return $ Just (ptr, reverse acc)

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let !ptr'   =  S.plusPtr ptr 1
                case chr $ fromIntegral w of
                 '"'    -> return $ Just (ptr', reverse acc)
                 '\\'   -> go_escape ptr' acc
                 c      -> go_body   ptr' (c : acc)

        -- Handle escaped character.
        -- The previous character was a '\\'
        go_escape !ptr !acc
         | ptr >= end
         = return Nothing

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
                 _      -> return Nothing
{-# NOINLINE unpackString #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

