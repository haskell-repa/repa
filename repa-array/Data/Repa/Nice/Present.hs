{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Data.Repa.Nice.Present
        ( Presentable (..)
        , depth
        , strip1
        , strip2
        , flatten)
where
import Data.List
import Data.Word


-- | A value, wrapped up nicely.
data Present
        = Atom  String          -- ^ An atomic thing.
        | Many  [Present]       -- ^ Many of the same thing.
        | Some  [Present]       -- ^ Some different things.
        deriving (Eq, Show)


-- | Yield the nesting depth of a `Present`
depth :: Present -> Int
depth pp
 = case pp of
        Atom{}   -> 0
        Many ps  -> 1 + (case ps of
                                []      -> 0
                                _       -> maximum $ map depth ps)
        Some _   -> 0


-- | Strip the top two layers of nesting into lists.
strip2 :: Present -> Maybe [[Present]]
strip2 (Many xs)
        = mapM strip1 xs


-- | Strip the top layer of nesting into a list.
strip1 :: Present -> Maybe [Present]
strip1 (Many xs) = Just xs
strip1 _         = Nothing


-- | Flatten a present into a string.
flatten :: Present -> String
flatten (Atom str) = str
flatten (Many ps)  = "[" ++ (intercalate "," $ map flatten ps) ++ "]"
flatten (Some ps)  = "(" ++ (intercalate "," $ map flatten ps) ++ ")"


-- | Convert some value to a form presentable to the user.
--   
--   Like `show` but we allow the nesting structure to be preserved
--   so it can be displayed in tabular format.
--
class Presentable a where
 present :: a -> Present 

instance Presentable Char where
 present = Atom . show

instance Presentable String where
 present = Atom . show

instance Presentable Int where
 present = Atom . show

instance Presentable Float where
 present = Atom . show

instance Presentable Double where
 present = Atom . show

instance Presentable Word8 where
 present = Atom . show

instance Presentable Word16 where
 present = Atom . show

instance Presentable Word32 where
 present = Atom . show

instance Presentable Word64 where
 present = Atom . show

instance Presentable a 
      => Presentable [a] where
 present xs = Many $ map present xs

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

