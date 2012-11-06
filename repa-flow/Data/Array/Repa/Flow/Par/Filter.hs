
module Data.Array.Repa.Flow.Par.Filter
        ( packByTag
        , pack
        , filter)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Map
import Data.Array.Repa.Flow.Par.Distro
import qualified Data.Array.Repa.Flow.Seq.Filter        as Seq
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Array.Repa.Eval.Gang              as Gang
import GHC.Exts
import Prelude hiding (map, filter)

-------------------------------------------------------------------------------
-- | Produce only the elements that have their corresponding flag set to `True`.
packByTag 
        :: U.Unbox a
        => Flow rep bal (Int, a) -> Flow rep BN a

packByTag (Flow _ frag)
 = Flow distro' frag'
 where
        !(I# threads)   = Gang.gangSize Gang.theGang
        distro'         = unbalanced threads
        frag' n         = Seq.packByTag (frag n)
{-# INLINE [2] packByTag #-}


------------------------------------------------------------------------------
-- | Produce only those elements that have their corresponding flag set.
pack    :: U.Unbox a 
        => Flow rep bal (Bool, a) -> Flow rep BN a
pack ff = packByTag $ map (\(b, x) -> (if b then 1 else 0, x)) ff
{-# INLINE [2] pack #-}


-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter :: U.Unbox a => (a -> Bool) -> Flow rep bal a -> Flow rep BN a
filter f ff
        = pack $ map (\x -> (f x, x)) ff
{-# INLINE [2] filter #-}
