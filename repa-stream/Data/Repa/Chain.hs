
module Data.Repa.Chain  
        ( -- * Chain fusion
          MChain (..), Chain
        , Step   (..)
        , liftChain
        , resume
        , chainOfStream
        , unchainToMVector

        -- * Grouping
        , groupsByC)
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Group
