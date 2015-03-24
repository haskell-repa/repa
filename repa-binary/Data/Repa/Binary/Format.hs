
module Data.Repa.Binary.Format
        ( Format (..)

        -- * Atomic formats
        , Int8be
        , Int16be
        , Int32be
        , Int64be

        -- * Products
        , (:*:)(..)

        -- * Lists
        , FixList(..)
        , VarList(..)

        -- * Strings
        , FixString (..)
        , ASCII)
where
import qualified Data.Int       as V


-------------------------------------------------------------------------------
-- | Relates storage formats with the type of value that is stored.
class Format f where

 -- | Get the type of a value with this format.
 type Value f  

 -- | Get the size of a value of of this format in bytes,
 --   or `Nothing` if the size is indeterminate.
 size :: f -> Maybe Int


-------------------------------------------------------------------------------
-- | Big-endian 8-bit signed integer.
data Int8be     = Int8be                deriving (Eq, Show)
instance Format Int8be                  where
 type Value Int8be      = V.Int8
 size _                 = Just 1

-- | Big-endian 16-bit signed integer.
data Int16be    = Int16be               deriving (Eq, Show)
instance Format Int16be                 where
 type Value Int16be     = V.Int16
 size _                 = Just 2

-- | Big-endian 32-bit signed integer.
data Int32be    = Int32be               deriving (Eq, Show)
instance Format Int32be                 where
 type Value Int32be     = V.Int32
 size _                 = Just 4

-- | Big-endian 64-bit signed integer.
data Int64be    = Int64be               deriving (Eq, Show)
instance Format Int64be                 where
 type Value Int64be     = V.Int64
 size _                 = Just 8


-------------------------------------------------------------------------------
-- | Generic product type.
data a :*: b    = !a :*: !b             deriving (Eq, Show)

instance (Format a, Format b) 
       => Format (a :*: b) where
 type Value (a :*: b) = Value a :*: Value b
 size (a :*: b)
  = do  sa      <- size a
        sb      <- size b
        return  $  sa + sb


-------------------------------------------------------------------------------
-- | Fixed length list.
data FixList   t = FixList   t Int      deriving (Eq, Show)
instance Format t => Format (FixList   t) where
 type Value (FixList t)         = [t]
 size (FixList t len)           
  = do  lenElem <- size t
        return  $ lenElem * len


-- | Variable length list.
data VarList   t = VarList   t          deriving (Eq, Show)
instance Format (VarList t)             where
 type Value (VarList t)         = [t]
 size (VarList _)               = Nothing


-------------------------------------------------------------------------------
-- | Fixed length string.
data FixString t = FixString t Int      deriving (Eq, Show)
instance Format (FixString ASCII)       where
 type Value (FixString ASCII)   = String
 size (FixString ASCII len)     = Just len


-- | Variable length string.
data VarString t = VarString t          deriving (Eq, Show)
instance Format (VarString ASCII)       where
 type Value (VarString ASCII)   = String
 size (VarString ASCII)         = Nothing


-- | String is encoded as 8-bit ASCII characters.
data ASCII       = ASCII                deriving (Eq, Show)

