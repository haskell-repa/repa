
module Data.Array.Repa.Series.Rate
        ( RateNat       (..)
        , rateOfRateNat

        , Down4         (..)
        , Tail4         (..))
where
import GHC.Exts

-- | Holds the value-level version of a type-level rate variable.
--
--   All the functions in the repa-series API ensures that the value-level and
--   type-level rates match up. 
--
--   Should be treated abstactly by user code.
--
data RateNat k
        = RateNat Word#
        deriving Show


rateOfRateNat :: RateNat k -> Word#
rateOfRateNat (RateNat w) = w
{-# INLINE rateOfRateNat #-}


-- | Represents the quotient of a rate divided by 4.
--
--   Should be treated abstactly by user code.
--
data Down4 k
        = Down4 Word#
        deriving Show

-- | Represents the remainder of a rate divided by 4.
--
--   Should be treated abstactly by user code.
--
data Tail4 k
        = Tail4 Word#
        deriving Show


