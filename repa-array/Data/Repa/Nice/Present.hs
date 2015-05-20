{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Repa.Nice.Present
        ( Presentable   (..)
        , Present       (..)
        , Str           (..)
        , Tok           (..)
        , depth
        , strip1
        , strip2
        , flatten)
where
import Data.Monoid
import Data.Word
import Data.Text                                (Text)
import Data.Repa.Nice                           (Str(..), Tok(..))
import Data.Repa.Scalar.Product                 ((:*:) (..))
import Data.Repa.Scalar.Date32                  (Date32)
import qualified Data.Text                      as T
import qualified Data.Repa.Scalar.Date32        as Date32
import Prelude                                  as P


-- | A value, wrapped up nicely.
data Present
        -- | Nothing to present.
        = Blank

        -- | An atomic thing.
        | Atom  Text

        -- | Many of the same thing, to display with list brackets @[.. , ..]@
        | Many  [Present]

        -- | Some different things,  to display with tuple brackets @(.. , ..)@
        | Some  [Present]
        deriving (Eq, Show)


-- | Yield the nesting depth of a `Present`
depth :: Present -> Int
depth pp
 = case pp of
        Blank{}  -> 0
        Atom{}   -> 0
        Many ps  -> 1 + (case ps of
                                []      -> 0
                                _       -> maximum $ map depth ps)
        Some _   -> 0


-- | Strip the top two layers of nesting into lists.
strip2 :: Present -> Maybe [[Present]]
strip2 (Many xs) = mapM strip1 xs
strip2 _         = Nothing


-- | Strip the top layer of nesting into a list.
strip1 :: Present -> Maybe [Present]
strip1 (Many xs) = Just xs
strip1 _         = Nothing


-- | Flatten a present into text
flatten :: Present -> Text 
flatten Blank      = T.pack ""
flatten (Atom str) = str
flatten (Many ps)  
 = T.pack "[" <> (T.intercalate (T.pack ",") $ map flatten ps) <> T.pack "]"

flatten (Some ps)  
 = T.pack "(" <> (T.intercalate (T.pack ",") $ map flatten ps) <> T.pack ")"


-- | Convert some value to a form presentable to the user.
--   
--   Like `show` but we allow the nesting structure to be preserved
--   so it can be displayed in tabular format.
--
class Presentable a where
 present :: a -> Present 


instance Presentable () where
 present _ = Blank


instance Presentable Char where
 present = Atom . T.pack . show


instance Presentable Int where
 present = Atom . T.pack . show


instance Presentable Float where
 present = Atom . T.pack . show


instance Presentable Double where
 present = Atom . T.pack . show


instance Presentable Word8 where
 present = Atom . T.pack . show


instance Presentable Word16 where
 present = Atom . T.pack . show


instance Presentable Word32 where
 present = Atom . T.pack . show


instance Presentable Word64 where
 present = Atom . T.pack . show


instance Presentable Date32 where
 present d
  | (yy, mm, dd)        <- Date32.unpack d
  = let cSep    = '/'
        yy'     = show yy       
        mm'     = if mm < 10 then "0" ++ show mm else show mm
        dd'     = if dd < 10 then "0" ++ show dd else show dd
    in  Atom $ T.pack $ P.concat [yy', [cSep], mm', [cSep], dd']


instance Presentable Str where
 present (Str xs) = Atom $ T.pack (show xs)


instance Presentable Tok where
 present (Tok xs) = Atom $ T.pack xs


instance Presentable a 
      => Presentable [a] where
 present xs = Many $ map present xs


instance (Presentable a, Presentable b)
       => Presentable (a :*: b) where

 present (xa :*: xb)
  = let aa      = case present xa of
                        Blank    -> []
                        Atom x   -> [Atom x]
                        Many xx  -> xx
                        Some xx  -> xx

        bb      = case present xb of
                        Blank    -> []
                        Atom x   -> [Atom x]
                        Many xx  -> xx
                        Some xx  -> xx
  in  Some (aa ++ bb)


instance (Presentable a, Presentable b)
       => Presentable (a, b) where
 present (a, b) 
        = Some [present a, present b]


instance (Presentable a, Presentable b, Presentable c)
       => Presentable (a, b, c) where
 present (a, b, c) 
        = Some [present a, present b, present c]


instance (Presentable a, Presentable b, Presentable c, Presentable d)
       => Presentable (a, b, c, d) where
 present (a, b, c, d)
        = Some [present a, present b, present c, present d]


instance (Presentable a, Presentable b, Presentable c, Presentable d, Presentable e)
       => Presentable (a, b, c, d, e) where
 present (a, b, c, d, e) 
        = Some [present a, present b, present c, present d, present e]

