
module Data.Repa.Nice.Display
        ( Display (..)
        , Format  (..)
        , display
        , takeDisplay
        , padL
        , padR)
where
import Data.Monoid
import Data.Char
import Data.Text                (Text)
import qualified Data.Text      as T

-- | How a given value should be displayed.
data Display
        = Display Format Int
        deriving (Eq, Show)


-- | Common display formats.
data Format
        = FormatNumeric 
        | FormatText
        deriving (Eq, Show)


instance Monoid Display where
 mempty  = Display FormatNumeric 0

 mappend (Display m1 len1) (Display m2 len2)
  | m1 == FormatNumeric && m2 == FormatNumeric
  = Display FormatNumeric (max len1 len2)

  | otherwise
  = Display FormatText    (max len1 len2)


-- | Display a string with the given mode.
display :: Display -> Text -> Text
display (Display FormatNumeric width) str
        = padR width str

display (Display FormatText    width) str
        = padL width str


-- | Examine a string to decide how we should display it.
takeDisplay :: Text -> Display
takeDisplay str
        | all (\c -> isDigit c || c == '.') $ T.unpack str
        = Display FormatNumeric (T.length str)

        | otherwise
        = Display FormatText    (T.length str)


-- | Left justify some text in a column of the given width.
padL n xs
 = let len = T.length xs
   in  if len >= n 
        then xs
        else xs <> T.replicate (n - len) (T.pack " ")


-- | Right justify some text in a column of the given width.
padR n xs
 = let len = T.length xs
   in  if len >= n 
        then xs
        else T.replicate (n - len) (T.pack " ") <> xs
