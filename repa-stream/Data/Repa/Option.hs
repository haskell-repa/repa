
-- | Data types used during low-level fusion optimisations.
-- 
--   These types are synonyms for @Maybe (a, b)@, which are strict in the
--   components. They can be used to ensure that we do not suspend the
--   computation that produces these components in fused code.
--
module Data.Repa.Option
        ( -- * Single component
          Option  (..)
        , fromOption,  toOption

          -- * Two components
        , Option2 (..)
        , fromOption2, toOption2

          -- * Three components
        , Option3 (..)
        , fromOption3, toOption3)
where


-------------------------------------------------------------------------------
-- | A strict `Maybe` type.
data Option a
        = Some !a
        | None 
        deriving Show

-- | Convert a `Maybe` to an `Option`.
toOption :: Maybe a -> Option a
toOption Nothing        = None
toOption (Just x)       = Some x
{-# INLINE toOption #-}


-- | Convert an `Option` to a `Maybe`.
fromOption :: Option a -> Maybe a
fromOption None         = Nothing
fromOption (Some x)     = Just x
{-# INLINE fromOption #-}


-------------------------------------------------------------------------------
-- | A strict `Maybe` type, with two parameters.
data Option2 a b
        = Some2 !a !b
        | None2 
        deriving Show


-- | Convert a `Maybe` to an `Option2`.
toOption2 :: Maybe (a, b) -> Option2 a b
toOption2 Nothing        = None2
toOption2 (Just (x, y))  = Some2 x y
{-# INLINE toOption2 #-}


-- | Convert an `Option2` to a `Maybe`.
fromOption2 :: Option2 a b -> Maybe (a, b)
fromOption2 None2        = Nothing
fromOption2 (Some2 x y)  = Just (x, y)
{-# INLINE fromOption2 #-}


-------------------------------------------------------------------------------
-- | A strict `Maybe` type with three parameters.
data Option3 a b c
        = Some3 !a !b !c
        | None3 
        deriving Show


-- | Convert a `Maybe` to an `Option3`.
toOption3 :: Maybe (a, b, c) -> Option3 a b c
toOption3 Nothing          = None3
toOption3 (Just (x, y, z)) = Some3 x y z
{-# INLINE toOption3 #-}


-- | Convert an `Option2` to a `Maybe`.
fromOption3 :: Option3 a b c -> Maybe (a, b, c)
fromOption3 None3          = Nothing
fromOption3 (Some3 x y z)  = Just (x, y, z)
{-# INLINE fromOption3 #-}

