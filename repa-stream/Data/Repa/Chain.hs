
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
        , groupsByC)
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Scan
