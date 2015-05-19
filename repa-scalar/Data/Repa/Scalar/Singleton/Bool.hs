{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Data.Repa.Scalar.Singleton.Bool
        ( B             (..)
        , Boolean       (..))
where

data B = Y | N

deriving instance Show B


data Boolean (b :: B) where
        Yes     :: Boolean Y
        No      :: Boolean N

deriving instance Show (Boolean b)


