
-- | * See the "Data.Repa.Vector.Unboxed" module for examples of how these
--     functions can be used.
module Data.Repa.Chain  
        ( -- * Chain Fusion
          Chain  (..)
        , Step   (..)
        , liftChain
        , resumeChain

        -- * Weaves
        , weaveC, Weave, Turn (..), Move(..), move

        -- * Folding
        , foldsC, Folds(..)

        -- * Unfolding
        , unfoldsC

        -- * Scanning
        , scanMaybeC

        -- * Grouping
        , groupsByC)
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Scan
import Data.Repa.Chain.Weave
import Data.Repa.Chain.Folds
