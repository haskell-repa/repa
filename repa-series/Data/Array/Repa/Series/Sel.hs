
module Data.Array.Repa.Series.Sel
        ( Sel1 (..)
        , mkSel1)
where
import Data.Array.Repa.Series.Series    as S
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

-- | Selectors.
data Sel1 k1 k2
        = Sel1
        { sel1Length    :: Int#
        , sel1Flags     :: !(U.Vector Bool) }


-- | Create a new selector from a series of flags.
mkSel1  :: Series k1 Bool 
        -> (forall k2. Sel1 k1 k2 -> a)
        -> a

mkSel1 sFlags worker
 = let  sel1    = Sel1
                { sel1Length    = S.length sFlags
                , sel1Flags     = S.seriesVector sFlags }
   in   worker sel1
{-# INLINE [1] mkSel1 #-}
