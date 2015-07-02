
module Data.Repa.Array.Generic.Target
        ( Target (..),          TargetI
        , empty,                singleton
        , fromList,             fromListInto
        , generateMaybeS,       mapMaybeS
        , generateEitherS,      mapEitherS)
where
import Data.Repa.Array.Internals.Target