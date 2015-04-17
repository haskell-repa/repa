
module Data.Repa.Query.Format
        ( Row   (..)
        , Field (..)
        , FormatField
        , makeFormatField
        , showField
        , readField)
where
import Data.Char
import qualified Data.Repa.Convert.Format       as C
import qualified Data.Map                       as Map


-- | Row format.
data Row
        -- | Packed binary data with fixed-width elements.
        = RowPacked  [Field]

        -- | Possibly varible-length row with the given character to separate the fields.
        | RowLineSep Char [Field]
        deriving (Eq, Show)


-- | Field formats supported by Repa tables.
--
--   The repa-convert library defines several data formats, and a singleton
--   type to represent each format. We enumerate all the formats here in a 
--   closed data type to make it easier to write parsers and pretty pritners
--   for queries that mention them.
--
data Field
        = Word8be               -- ^ Big-endian 8-bit unsigned word.
        | Int8be                -- ^ Big-endian 8-bit signed integer.

        | Word16be              -- ^ Big-endian 16-bit unsigned word.
        | Int16be               -- ^ Big-endian 16-bit signed integer.

        | Word32be              -- ^ Big-endian 32-bit unsigned word.
        | Int32be               -- ^ Big-endian 32-bit signed integer.

        | Word64be              -- ^ Big-endian 64-bit unsigned word.
        | Int64be               -- ^ Big-endian 64-bit signed integer.

        | Float32be             -- ^ Big-endian 32-bit IEEE 754 float.
        | Float64be             -- ^ Big-endian 64-bit IEEE 754 float.

        | YYYYsMMsDD Char       -- ^ Date in ASCII YYYYsMMsDD format.
        | DDsMMsYYYY Char       -- ^ Date in ASCII DDsMMsYYYY format.

        | IntAsc                -- ^ Human readable ASCII integer.
        | DoubleAsc             -- ^ Human readable ASCII double.

        | FixAsc     Int        -- ^ Fixed length ASCII string.
        | VarAsc                -- ^ Variable length ASCII string.
        deriving (Eq, Show)



---------------------------------------------------------------------------------------------------
-- | Show a field format.
showField :: Field -> String
showField ff = show ff


-- | Parse a field format.
readField :: String -> Maybe Field
readField ss
 | Just f       <- Map.lookup ss atomic
 = Just f

 | ["YYYYsMMsDD", sep]  <- words ss
 , ['\'', c, '\'']      <- sep
 = Just $ YYYYsMMsDD c

 | ["DDsMMsYYYY", sep]  <- words ss
 , ['\'', c, '\'']      <- sep
 = Just $ DDsMMsYYYY c

 | ["FixAsc", sLen]     <- words ss
 , all isDigit sLen
 = Just $ FixAsc (read sLen)

 | otherwise
 = Nothing

 where
        atomic
         = Map.fromList
         [ ("Word8be",    Word8be),      ("Int8be",  Int8be)
         , ("Word16be",   Word16be),     ("Int16be", Int16be)
         , ("Word32be",   Word32be),     ("Int32be", Int32be)
         , ("Word64be",   Word64be),     ("Int64be", Int64be)
         , ("Float32be",  Float32be)
         , ("Float64be",  Float64be)
         , ("IntAsc",     IntAsc)
         , ("DoubleAsc",  DoubleAsc)
         , ("VarAsc",     VarAsc) ]


---------------------------------------------------------------------------------------------------
-- | Existential container for field formats,
--   and dictionaries to work with them.
data FormatField
        =  forall format
        .  (C.Packable format, Show format) 
        => FormatField format


-- | Package up the dictionaries for a given format.
makeFormatField :: Field -> FormatField
makeFormatField ff
 = case ff of
        Word8be         -> FormatField C.Word8be
        Int8be          -> FormatField C.Int8be

        Word16be        -> FormatField C.Word16be
        Int16be         -> FormatField C.Int16be

        Word32be        -> FormatField C.Word32be
        Int32be         -> FormatField C.Int32be

        Word64be        -> FormatField C.Word64be
        Int64be         -> FormatField C.Int64be

        Float32be       -> FormatField C.Word8be
        Float64be       -> FormatField C.Int8be

        YYYYsMMsDD c    -> FormatField (C.YYYYsMMsDD c)
        DDsMMsYYYY c    -> FormatField (C.DDsMMsYYYY c)

        IntAsc          -> FormatField C.IntAsc
        DoubleAsc       -> FormatField C.DoubleAsc

        FixAsc len      -> FormatField (C.FixAsc len)
        VarAsc          -> FormatField C.VarAsc

