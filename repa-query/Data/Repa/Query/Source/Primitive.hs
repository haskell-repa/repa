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
        , Query
        , Flow 
        , Value

        , query

        , fromFile
        , fromTable
        , fromTableColumns

        , map
        , fold, folds
        , filter
        , groups, groupsBy

        , negate, abs, signum
        , (+),  (-),  (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=)

        , get2_1, get2_2
        , get3_1, get3_2, get3_3
        , get4_1, get4_2, get4_3, get4_4
        , get5_1, get5_2, get5_3, get5_4, get5_5

          -- * From Data.Repa.Query.Format
        , Delim (..)
        , Field (..))
where
import Data.Repa.Query.Source.EDSL
import Data.Repa.Store.Format
import qualified Prelude                as P


($)     = (\x y -> x P.$ y)
return  = P.return
(>>=)   = (\x y -> x P.>>= y)
(=<<)   = (\x y -> x P.=<< y)


