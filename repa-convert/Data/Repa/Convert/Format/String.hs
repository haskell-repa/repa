
module Data.Repa.Convert.Format.String
        ( -- * Haskell Strings
          FixChars      (..)
        , VarChars      (..)
        , VarCharString (..)
        , ExactChars    (..)
        , unpackCharList)
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
-- | Fixed length sequence of characters, represented as a (hated) Haskell `String`.
--   
-- * The runtime performance of the Haskell `String` is atrocious.
--   You really shouldn't be using them for large data sets.
--
-- * When packing, the length of the provided string must match the width
--   of the format, else packing will fail.
--
-- * When unpacking, the length of the result will be the width of the format.
--
data FixChars                   = FixChars Int          deriving (Eq, Show)
instance Format FixChars where
 type Value (FixChars)          = String
 fieldCount _                   = 1
 minSize    (FixChars len)      = len
 fixedSize  (FixChars len)      = Just len
 packedSize (FixChars len) _    = Just len
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable FixChars where
 
  pack (FixChars len) xs 
   |  length xs == len
   =  Packer $ \dst _fails eat
   -> do mapM_ (\(o, x) -> S.pokeByteOff (Ptr dst) o (w8 $ ord x)) 
                $ zip [0 .. len - 1] xs
         let !(Ptr dst') = S.plusPtr (Ptr dst) len
         eat dst'

   | otherwise
   = Packer $ \_ fails _ -> fails
  {-# NOINLINE pack #-}

  packer f v 
   = fromPacker (pack f v)
  {-# INLINE packer #-}

  unpacker (FixChars len@(I# len')) start end _stop fail eat
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
-- | Like `FixChars`, but with a variable length.
data VarChars = VarChars        deriving (Eq, Show)
instance Format VarChars        where
 type Value VarChars            = String

 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 0
 {-# INLINE minSize    #-}

 fixedSize  VarChars            = Nothing
 {-# INLINE fixedSize  #-}

 packedSize VarChars xs         = Just $ length xs
 {-# NOINLINE packedSize #-}


instance Packable VarChars where

  pack VarChars xx
   = case xx of
        []       -> mempty
        (x : xs) -> pack Word8be (w8 $ ord x) <> pack VarChars xs     
                                -- TODO: will leak, can't elim <>
  {-# NOINLINE pack #-}

  packer f v 
   = fromPacker (pack f v)
  {-# INLINE packer #-}

  unpacker VarChars start end stop _fail eat
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
-- | Variable length string in double quotes,
--   and standard backslash encoding of non-printable characters.
data VarCharString = VarCharString      deriving (Eq, Show)
instance Format VarCharString           where
 type Value VarCharString       = String
 fieldCount _                   = 1
 {-# INLINE fieldCount #-}

 minSize    _                   = 2
 {-# INLINE minSize #-}

 fixedSize  _                   = Nothing
 {-# INLINE fixedSize #-}
 
 packedSize VarCharString xs        
  = Just $ length $ show xs     
 {-# NOINLINE packedSize #-}


instance Packable VarCharString where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 packer     VarCharString xx          start k
  =  packer VarChars (show xx) start k
 {-# INLINE pack #-}

 unpacker   VarCharString start end _stop  fail eat
  = unpackString (pw8 start) (pw8 end) fail eat
 {-# INLINE unpacker #-}


-- | Unpack a string from the given buffer.
--   TODO: this isn't complete for all special characters, 
--         check the Haskell encoding.
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
-- | Match an exact sequence of characters.
data ExactChars
        = ExactChars String
        deriving Show


instance Format ExactChars where
 type Value ExactChars          = ()
 fieldCount (ExactChars _)      = 0
 {-# INLINE   fieldCount #-}

 minSize    (ExactChars str)    = length str
 {-# NOINLINE minSize  #-}

 fixedSize  (ExactChars str)    = return (length str)
 {-# NOINLINE fixedSize #-}

 packedSize (ExactChars str) () = return (length str)
 {-# NOINLINE packedSize #-}


instance Packable ExactChars where
 packer (ExactChars str) _ dst _fails k
  = do  let !len = length str
        mapM_ (\(o, x) -> S.pokeByteOff (Ptr dst) o (w8 $ ord x))
                $ zip [0 .. len - 1] str
        let !(Ptr dst') = S.plusPtr (Ptr dst) len
        k dst'
 {-# NOINLINE pack #-}

 unpacker (ExactChars str) start end _stop fails eat
  = do  let !len@(I# len') = length str
        let !lenBuf        = I# (minusAddr# end start)
        if  lenBuf < len
         then fails
         else do
                let load_unpackChar o
                      = do x :: Word8 <- S.peekByteOff (pw8 start) o
                           return $ chr $ fromIntegral x
                    {-# INLINE load_unpackChar #-}

                xs      <- mapM load_unpackChar [0 .. len - 1]
                if (xs == str)
                 then eat (plusAddr# start len') ()
                 else fails
 {-# NOINLINE unpack #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
