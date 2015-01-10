
module Data.Repa.Fusion.Option
        ( Option  (..)
        , Option2 (..))
where


data Option a
        = Some !a
        | None 
        deriving Show


data Option2 a b
        = Some2 !a !b
        | None2 
        deriving Show
