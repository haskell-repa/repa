
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


-- | How a given value should be displayed.
data Display
        = Display Format Int


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
display :: Display -> String -> String
display (Display FormatNumeric width) str
        = padR width str

display (Display FormatText    width) str
        = padL width str


-- | Examine a string to decide how we should display it.
takeDisplay :: String -> Display
takeDisplay str
        | all (\c -> isDigit c || c == '.') str
        = Display FormatNumeric (length str)

        | otherwise
        = Display FormatText    (length str)

padL n xs
 = let len = length xs
   in  if len >= n 
        then xs
        else xs ++ replicate (n - len) ' '

padR n xs
 = let len = length xs
   in  if len >= n 
        then xs
        else replicate (n - len) ' ' ++ xs
