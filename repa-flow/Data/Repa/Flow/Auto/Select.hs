
module Data.Repa.Flow.Auto.Select
        ( select_i,     select_o
        , discard_i,    discard_o
        , mask_i,       mask_o)
where
import Data.Repa.Array                          as A
import Data.Repa.Flow.Auto.Base                 as F
import Data.Repa.Scalar.Singleton.Nat           as D
import Data.Repa.Scalar.Product                 as D
import qualified Data.Repa.Flow.Generic         as G
#include "repa-array.h"


---------------------------------------------------------------------------------------------------
-- | Select a single column from a flow of rows of fields.
select_i 
        :: ( Select  n (Array fs)
           , Select' n (Array fs) ~ Array (Select' n fs))
        => Nat n                        -- ^ Index of column to keep.
        -> Sources fs                   -- ^ Sources of complete rows.
        -> IO (Sources (Select' n fs))  -- ^ Sources of selected column.

select_i n ss
        = G.map_i (select n) ss
{-# INLINE_FLOW select_i #-}


-- | Select a single column from a flow of fields.
select_o
        :: ( Select  n (Array fs)
           , Select' n (Array fs) ~ Array (Select' n fs))
        => Nat n                        -- ^ Index of column to keep.
        -> Sinks (Select' n fs)         -- ^ Sinks for selected column.
        -> IO (Sinks fs)                -- ^ Sinks for complete rows. 

select_o n ss
        = G.map_o (select n) ss
{-# INLINE_FLOW select_o #-}
        

---------------------------------------------------------------------------------------------------
-- | Discard a single column from a flow of fields.
discard_i
        :: ( Discard  n (Array fs)
           , Discard' n (Array fs) ~ Array (Discard' n fs))
        => Nat n                        -- ^ Index of column to discard.
        -> Sources fs                   -- ^ Sources of complete rows.
        -> IO (Sources (Discard' n fs)) -- ^ Sources of partial  rows.

discard_i n ss
        = G.map_i (discard n) ss
{-# INLINE_FLOW discard_i #-}


-- | Discard a single column from a flow of fields.
discard_o
        :: ( Discard  n (Array fs)
           , Discard' n (Array fs) ~ Array (Discard' n fs))
        => Nat n                        -- ^ Index of column to discard.
        -> Sinks (Discard' n fs)        -- ^ Sinks for partial rows.
        -> IO (Sinks fs)                -- ^ Sinks for complete rows.

discard_o n ss
        = G.map_o (discard n) ss
{-# INLINE_FLOW discard_o #-}


---------------------------------------------------------------------------------------------------
-- | Mask columns from a flow of fields.
mask_i  :: ( Mask  ms (Array fs)
           , Mask' ms (Array fs) ~ Array (Mask' ms fs))
        => ms                           -- ^ Column mask.
        -> Sources fs                   -- ^ Sources of complete rows.
        -> IO (Sources (Mask' ms fs))   -- ^ Sources of masked rows.

mask_i ms ss
        = G.map_i (mask ms) ss
{-# INLINE_FLOW mask_i #-}


-- | Mask columns from a flow of fields.
mask_o  :: ( Mask  ms (Array fs)
           , Mask' ms (Array fs) ~ Array (Mask' ms fs))
        => ms                           -- ^ Column mask.
        -> Sinks (Mask' ms fs)          -- ^ Sources of complete rows.
        -> IO (Sinks fs)                -- ^ Sources of masked rows.

mask_o ms ss
        = G.map_o (mask ms) ss
{-# INLINE_FLOW mask_o #-}

