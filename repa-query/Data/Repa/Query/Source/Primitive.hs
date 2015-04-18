{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Primitive functions used by query source code when producing the operator graph.
--   All named used by the source are defined here so we can keep track
--   of what is being used.
--
module Data.Repa.Query.Source.Primitive
        ( -- * From Prelude
          ($)
        , return, (>>=), (=<<)
        , P.Int, P.Float, P.Double, P.String

          -- * From Data.Repa.Query.Source.EDSL
        , Flow 
        , Value
        , source
        , map
        , fold, folds
        , filter
        , groups, groupsBy

        , (+),  (-),  (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=)

        , get1, get2, get3, get4, get5

          -- * From Data.Repa.Query.Source.Builder
        , query

          -- * From Data.Repa.Query.Format
        , Delim (..)
        , Field (..))
where
import Data.Repa.Query.Source.EDSL
import Data.Repa.Query.Source.Builder
import Data.Repa.Query.Format
import qualified Prelude                as P


($)     = (\x y -> x P.$ y)
return  = P.return
(>>=)   = (\x y -> x P.>>= y)
(=<<)   = (\x y -> x P.=<< y)


