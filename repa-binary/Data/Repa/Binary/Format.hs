
module Data.Repa.Binary.Format
        ( Format (..)

        -- * Atomic formats
        , Word8be   (..),        Int8be  (..)
        , Word16be  (..),        Int16be (..)
        , Word32be  (..),        Int32be (..)
        , Word64be  (..),        Int64be (..)

        , Float32be (..)
        , Float64be (..)

        -- * Lists
        , FixList(..)
        , VarList(..)

        -- * Strings
        , FixString     (..)
        , VarString     (..)
        , ASCII         (..))
where
import Data.Repa.Binary.Product
import qualified Data.Int       as V
import qualified Data.Word      as V


-------------------------------------------------------------------------------
-- | Relates storage formats with the type of value that is stored.
class Format f where

 -- | Get the type of a value with this format.
 type Value f  

 -- | If the given format has a fixed size then return it, 
 --   otherwise `Nothing`.
 fixedSize  :: f -> Maybe Int

 -- | Yield the size of a value of the given format.
 --
 --   This will return `Nothing` when a collection is packed into
 --   a fixed length format, and the length of the provided collection
 --   does not match the format.
 --
 packedSize :: f -> Value f -> Maybe Int


-------------------------------------------------------------------------------
-- | Big-endian 8-bit signed integer.
data Int8be     = Int8be                deriving (Eq, Show)
instance Format Int8be                  where
 type Value Int8be      = V.Int8
 fixedSize  _           = Just 1
 packedSize _ _         = Just 1


-- | Big-endian 8-bit unsigned word.
data Word8be     = Word8be              deriving (Eq, Show)
instance Format Word8be                 where
 type Value Word8be     = V.Word8
 fixedSize  _           = Just 1
 packedSize _ _         = Just 1


-- | Big-endian 16-bit signed integer.
data Int16be    = Int16be               deriving (Eq, Show)
instance Format Int16be                 where
 type Value Int16be     = V.Int16
 fixedSize _            = Just 2
 packedSize _ _         = Just 2


-- | Big-endian 32-bit unsigned word.
data Word16be    = Word16be             deriving (Eq, Show)
instance Format Word16be                where
 type Value Word16be    = V.Word16
 fixedSize _            = Just 2
 packedSize _ _         = Just 2


-- | Big-endian 32-bit signed integer.
data Int32be    = Int32be               deriving (Eq, Show)
instance Format Int32be                 where
 type Value Int32be     = V.Int32
 fixedSize _            = Just 4
 packedSize _ _         = Just 4


-- | Big-endian 32-bit unsigned word.
data Word32be    = Word32be             deriving (Eq, Show)
instance Format Word32be                where
 type Value Word32be    = V.Word32
 fixedSize _            = Just 4
 packedSize _ _         = Just 4


-- | Big-endian 64-bit signed integer.
data Int64be    = Int64be               deriving (Eq, Show)
instance Format Int64be                 where
 type Value Int64be     = V.Int64
 fixedSize _            = Just 8
 packedSize _ _         = Just 8


-- | Big-endian 64-bit unsigned word.
data Word64be    = Word64be             deriving (Eq, Show)
instance Format Word64be                where
 type Value Word64be    = V.Word64
 fixedSize _            = Just 8
 packedSize _ _         = Just 8


-- | Big-endian 32-bit IEEE 754 float.
data Float32be  = Float32be             deriving (Eq, Show)
instance Format Float32be               where
 type Value Float32be   = Float
 fixedSize  _           = Just 4
 packedSize _ _         = Just 4


-- | Big-endian 64-bit IEEE 754 float.
data Float64be  = Float64be             deriving (Eq, Show)
instance Format Float64be               where
 type Value Float64be   = Double
 fixedSize  _           = Just 8
 packedSize _ _         = Just 8


-------------------------------------------------------------------------------
instance (Format a, Format b) 
       => Format (a :*: b) where
 type Value (a :*: b) = Value a :*: Value b

 fixedSize  (xa :*: xb)
  = do  sa      <- fixedSize xa
        sb      <- fixedSize xb
        return  $  sa + sb
 {-# INLINE fixedSize #-}

 packedSize (fa :*: fb) (xa :*: xb)
  = do  sa      <- packedSize fa xa
        sb      <- packedSize fb xb
        return  $  sa + sb
 {-# INLINE packedSize #-}


-------------------------------------------------------------------------------
-- | Fixed length list.
--
data FixList   f = FixList   f Int      deriving (Eq, Show)
instance Format f => Format (FixList   f) where
 type Value (FixList f)         = [Value f]

 fixedSize  (FixList f len)           
  = do  lenElem <- fixedSize f
        return  $ lenElem * len

 packedSize (FixList _ 0) _
  =     return 0

 packedSize (FixList f len) xs
  | length xs == len
  = do  lenElems <- mapM (packedSize f) xs
        return   $ sum lenElems

  | otherwise 
  = Nothing


-- | Variable length list.
data VarList   f = VarList   f          deriving (Eq, Show)
instance Format f => Format (VarList f) where
 type Value (VarList f)          = [Value f]

 fixedSize  (VarList _)          = Nothing

 packedSize (VarList f) xs@(x : _)
  = do  lenElem <- packedSize f x
        return  $ lenElem * length xs

 packedSize _ []
  =     return 0


-------------------------------------------------------------------------------
-- | Fixed length string.
--   
--   * When packing, if the provided string is shorter than the fixed length
--     then the extra bytes are zero-filled. 
--
data FixString t = FixString t Int      deriving (Eq, Show)
instance Format (FixString ASCII)       where
 type Value (FixString ASCII)       = String
 fixedSize  (FixString ASCII len)   = Just len
 packedSize (FixString ASCII len) _ = Just len


-- | Variable length string.
data VarString t = VarString t          deriving (Eq, Show)
instance Format (VarString ASCII)       where
 type Value (VarString ASCII)       = String
 fixedSize  (VarString ASCII)       = Nothing
 packedSize (VarString ASCII) xs    = Just $ length xs


-- | String is encoded as 8-bit ASCII characters.
data ASCII       = ASCII                deriving (Eq, Show)

