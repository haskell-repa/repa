
-- | Conversion between the open type-index representation of formats,
--   and closed data-type view. 
module Data.Repa.Store.Format
        ( Row   (..)
        , Delim (..)
        , Field (..)

        , FieldBox (..)
        , flattens
        , flattensBox

        , FieldFormat (..)
        
        , showField
        , readField)
where
import Control.Monad
import Data.Char
import Data.Word
import Data.Int
import Data.Text                                (Text)
import Data.Repa.Scalar.Date32                  as Date32
import Data.Aeson                               as Aeson
import qualified Data.Map                       as Map
import qualified Data.Repa.Scalar.Product       as P
import qualified Data.HashMap.Strict            as H
import qualified Data.Text                      as T

import qualified Data.Repa.Convert.Format       as F
import qualified Data.Repa.Convert.Formats      as F


---------------------------------------------------------------------------------------------------
-- | Row format.
data Row aa
        = Row Delim (Field aa)
        deriving (Eq, Show)


---------------------------------------------------------------------------------------------------
-- | How the rows and fields are delimited.
data Delim
        -- | Format with fixed-length rows.
        --   The fields are all concatenated together, with no delimitors.
        = Fixed

        -- | Format with a single field on each lines.
        | Lines

        -- | Format with multiple fields on each line,
        --   where the fields are separated by a special character.
        | LinesSep Char
        deriving (Eq, Show)


instance ToJSON Delim where
 toJSON rr
  = case rr of
        Fixed
         -> object [ "_type"    .= text "delim"
                   , "delim"    .= text "fixed" ]

        Lines
         -> object [ "_type"    .= text "delim"
                   , "delim"    .= text "lines" ]

        LinesSep c
         -> object [ "_type"    .= text "delim"
                   , "delim"    .= text "sep"
                   , "sep"      .= T.pack [c] ]

instance FromJSON Delim where
 parseJSON (Object hh)
        | Just (String "delim")  <- H.lookup "_type"  hh
        , Just (String "fixed")  <- H.lookup "delim"  hh
        =       return $ Fixed

        | Just (String "delim")  <- H.lookup "_type"  hh
        , Just (String "lines")  <- H.lookup "delim"  hh
        =       return $ Lines

        | Just (String "delim")  <- H.lookup "_type"  hh
        , Just (String "sep")    <- H.lookup "delim"  hh
        , Just (String sep)      <- H.lookup "sep"    hh
        , [c]                    <- T.unpack sep
        =       return $ LinesSep c

 parseJSON _ = mzero


---------------------------------------------------------------------------------------------------
-- | Field formats supported by Repa tables.
--
--   The repa-convert library defines several data formats using a singleton
--   type to represent each format. We enumerate all the formats here in a
--   closed data type to make it easier to write parsers and pretty pritners
--   for queries that mention them.
--
--   The names of the constructors have the same names as the ones in 
--   repa-convert, so when we read/show them they match.
--
data Field a where

        -- Compound field.
        (:*:)           :: Field x -> Field y -> Field (x P.:*: y)

        -- Empty row.
        Nil             :: Field ()

        -- Big-endian 8-bit unsigned word.
        Word8be         :: Field Word8

        -- Big-endian 8-bit signed integer.
        Int8be          :: Field Int8

        -- Big-endian 16-bit unsigned word.
        Word16be        :: Field Word16

        -- Big-endian 16-bit signed integer.
        Int16be         :: Field Int16

        -- Big-endian 32-bit unsigned word.
        Word32be        :: Field Word32

        -- Big-endian 32-bit signed integer.
        Int32be         :: Field Int32

        -- Big-endian 64-bit unsigned word.
        Word64be        :: Field Word64

        -- Big-endian 64-bit signed integer.
        Int64be         :: Field Int64

        -- Big-endian 32-bit IEEE 754 float.
        Float32be       :: Field Float

        -- Big-endian 64-bit IEEE 754 float.
        Float64be       :: Field Double

        -- Date in ASCII YYYYsMMsDD format.
        YYYYsMMsDD      :: Char -> Field Date32

        -- Date in ASCII DDsMMsYYYY format.
        DDsMMsYYYY      :: Char -> Field Date32

        -- Human readable ASCII integer.
        IntAsc          :: Field Int

        -- Human readable ASCII integer with leading zeros.
        IntAsc0         :: Int  -> Field Int

        -- Human readable ASCII double.
        DoubleAsc       :: Field Double

        -- Fixed length ASCII string.
        FixAsc          :: Int -> Field String

        -- Variable length ASCII string.
        VarAsc          :: Field String


deriving instance Show (Field a)
deriving instance Eq   (Field a)

infixr :*:

-- | Show a field format.
showField :: Field a -> String
showField ff = show ff


-- | Parse a field format.
readField :: String -> Maybe FieldBox
readField ss
 | Just f       <- Map.lookup ss atomic
 = Just f

 | ["YYYYsMMsDD", sep]  <- words ss
 , ['\'', c, '\'']      <- sep
 = Just $ FieldBox $ YYYYsMMsDD c

 | ["DDsMMsYYYY", sep]  <- words ss
 , ['\'', c, '\'']      <- sep
 = Just $ FieldBox $ DDsMMsYYYY c

 | ["IntAsc0", sLen]    <- words ss
 , all isDigit sLen
 = Just $ FieldBox $ IntAsc0 (read sLen)

 | ["FixAsc", sLen]     <- words ss
 , all isDigit sLen
 = Just $ FieldBox $ FixAsc (read sLen)

 | otherwise
 = Nothing

 where
        atomic
         = Map.fromList
         [ ("Nil",        FieldBox       Nil)
         , ("Word8be",    FieldBox   Word8be)
         , ("Int8be",     FieldBox    Int8be) 
         , ("Word16be",   FieldBox  Word16be)
         , ("Int16be",    FieldBox   Int16be)
         , ("Word32be",   FieldBox  Word32be)
         , ("Int32be",    FieldBox   Int32be)
         , ("Word64be",   FieldBox  Word64be)
         , ("Int64be",    FieldBox   Int64be)
         , ("Float32be",  FieldBox Float32be)
         , ("Float64be",  FieldBox Float64be)
         , ("IntAsc",     FieldBox    IntAsc)
         , ("DoubleAsc",  FieldBox DoubleAsc)
         , ("VarAsc",     FieldBox    VarAsc) ]


---------------------------------------------------------------------------------------------------
-- | Existential container for field formats,
--   and dictionaries to work with them.
data FieldBox
        =  forall a
        .  FieldBox (Field a)

instance Show FieldBox where
 show (FieldBox f)      = show f


instance ToJSON   FieldBox where
 toJSON (FieldBox ff)
        = toJSON ff

instance ToJSON (Field a) where
 toJSON ff
        = object  [ "_type"     .= text "field"
                  , "field"     .= T.pack (showField ff) ]


instance FromJSON FieldBox where
 parseJSON (Object hh)
        | Just (String "field") <- H.lookup "_type" hh
        , Just (String ff)      <- H.lookup "field" hh
        , Just f                <- readField (T.unpack ff)
        = return f

 parseJSON _ = mzero


-- | Flatten compound fields into their parts and box up the components.
flattens :: Field a -> [FieldBox]
flattens ff
 = case ff of
        (:*:) f1 f2     -> flattens f1 ++ flattens f2
        _               -> [FieldBox ff]


-- | Like `flattens`, but start with a boxed field format.
flattensBox :: FieldBox -> [FieldBox]
flattensBox (FieldBox f)
        = flattens f


---------------------------------------------------------------------------------------------------
class FieldFormat format where
 -- Convert a format from the open type version to the closed type version.
 fieldFormat :: format -> Field (F.Value format)


instance FieldFormat () where
 fieldFormat _    = Nil


instance ( FieldFormat f1
         , FieldFormat fs)
      =>   FieldFormat (f1 F.:*: fs)  where
 fieldFormat (f1 F.:*: fs)       
  = fieldFormat f1 :*: fieldFormat fs


instance FieldFormat F.IntAsc where
 fieldFormat _    = IntAsc


instance FieldFormat F.DoubleAsc where
 fieldFormat _    = DoubleAsc


instance FieldFormat F.VarChars where
 fieldFormat _    = VarAsc


---------------------------------------------------------------------------------------------------
text :: Text -> Text
text x = x 

