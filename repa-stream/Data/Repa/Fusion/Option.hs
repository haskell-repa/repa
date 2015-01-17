
module Data.Repa.Fusion.Option
        ( Option  (..)
        , Option2 (..)
        , Option3 (..))
where


data Option a
        = Some !a
        | None 
        deriving Show


data Option2 a b
        = Some2 !a !b
        | None2 
        deriving Show


data Option3 a b c
        = Some3 !a !b !c
        | None3 
        deriving Show
