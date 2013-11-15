
module Data.Array.Repa.Series.Sel
        ( Sel1 (..)
        , mkSel1)
where
import Data.Array.Repa.Series.Process
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Vector.Primitive            as P
import System.IO.Unsafe
import Data.Word
import GHC.Exts


-- | Selectors.
data Sel1 k1 k2
        = Sel1
        { sel1Length    :: Word#
        , sel1Flags     :: !(P.Vector Bool) }


-- | Create a new selector from a series of flags.
mkSel1  :: Series k1 Bool
        -> (forall k2. Sel1 k1 k2 -> Process)
        -> Process

mkSel1 (Series _ len _ vec) worker
 = worker       $ Sel1
                { sel1Length    = len
                , sel1Flags     = vec }
{-# NOINLINE mkSel1 #-}
