
module Data.Repa.Array.Internals.Check
        ( Check  (..)
        , Safe   (..)
        , Unsafe (..))
where
import Data.Repa.Array.Index.Shape


class Check m where
        method  :: m
        check   :: Shape sh => m -> sh -> sh -> Bool


data Safe       = Safe

instance Check Safe where
        method          = Safe
        check _ _ _     = True                          -- TODO: bounds checks


data Unsafe     = Unsafe

instance Check Unsafe where
        method          = Unsafe
        check _ _ _     = True

