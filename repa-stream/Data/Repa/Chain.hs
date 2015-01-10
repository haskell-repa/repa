
module Data.Repa.Chain  
        ( -- * Chain Fusion
          Chain  (..)
        , Step   (..)
        , liftChain
        , resumeChain

        -- * Scan Operators
        -- | These have a scan-like structure, 
        --   and are implemented in terms of `scanMaybeC`.
        , scanMaybeC

        -- ** Grouping
        , groupsByC

        -- * Weave Operators
        -- | These have a weave-like structure, 
        --   and are implemented in terms of `weaveC`.
        , weaveC, Weave, Turn (..), Move(..), move

        -- ** Folding
        , foldsC, Folds(..))
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Scan
import Data.Repa.Chain.Weave
import Data.Repa.Chain.Folds
import qualified Data.Vector.Fusion.Util as S
