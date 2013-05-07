
module Data.Array.Repa.Plugin.ToDDC.Detect.Base
        ( Detect  (..)
        , DetectS (..)
        , zeroState
        , collect
        , setRateVar
        , isRateVar)
where
import DDC.Core.Flow
import Data.Array.Repa.Plugin.FatName
import Data.Map                 (Map)
import Data.Set                 (Set)
import Control.Monad.State.Strict
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- Detect ---------------------------------------------------------------------
-- | Detect series operators in code converted from GHC Core, rewriting the raw
--   AST converted from GHC to be a well formed Disciple Core program. At the 
--   same time, remember the mapping from Disciple to GHC core names so we can
--   convert the transformed Disciple program back to GHC core.

--   After this pass the code should type check.
class Detect (c :: * -> *) where
 detect :: c FatName -> State DetectS (c Name)


-- Detect State ---------------------------------------------------------------
data DetectS     
        = DetectS
        { -- Map of Disciple Core names to GHC Core Names.
          stateNames    :: Map Name GhcName

          -- Names of rate variables, which we discover as they are arguments
          -- of Stream type constructors. 
          -- In GHC core rate variables have kind '*', 
          --   but for Disciple Core we change them to have kind 'Rate'.
        , stateRateVars :: Set Name }


zeroState :: DetectS
zeroState
        = DetectS
        { stateNames    = Map.empty
        , stateRateVars = Set.empty }


collect :: Name -> GhcName -> State DetectS ()
collect !d !g
 = modify $ \s -> s { stateNames    = Map.insert d g (stateNames s) }


setRateVar :: Name -> State DetectS ()
setRateVar !name
 = modify $ \s -> s { stateRateVars = Set.insert name (stateRateVars s) }


isRateVar  :: Name -> State DetectS Bool
isRateVar name
 = do   s       <- gets stateRateVars 
        return  $ Set.member name s
