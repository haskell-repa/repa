
module Data.Repa.Chain  
        ( -- * Chain fusion
          MChain (..), Chain
        , Step   (..)
        , liftChain
        , resume
        , chainOfVector
        , unchainToMVector

        -- * Scanning
        , scanMaybeC

        -- * Grouping
        , groupsByC

        -- * Weaving
        , weaveC, Weave, Turn (..), Move(..), move

        -- * Folding
        , foldsC, Folds(..), packFolds)
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Scan
import Data.Repa.Chain.Weave
import Data.Repa.Chain.Folds
