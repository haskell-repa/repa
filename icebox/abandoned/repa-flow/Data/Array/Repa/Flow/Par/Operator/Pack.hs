
module Data.Array.Repa.Flow.Par.Operator.Pack
        ( packByTag
        , packByFlag
        , filter)
where
import Data.Array.Repa.Bulk.Gang
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Par.Flow
import Data.Array.Repa.Flow.Par.Operator.Map
import Data.Array.Repa.Flow.Par.Distro
import qualified Data.Array.Repa.Flow.Seq.Operator.Pack as Seq
import GHC.Exts
import Prelude hiding (map, filter)


-------------------------------------------------------------------------------
-- | Produce only the elements that have their corresponding
--   flag set to `1`.
packByTag :: Unbox a
          => Flow FD dist (Int, a) -> Flow FD BN a

packByTag (Flow gang _ start frag)
 = Flow gang distro' start frag'
 where  !threads        = gangSize gang
        distro'         = unbalanced threads

        frag' state n   = Seq.packByTag_i (frag state n)
        {-# INLINE frag' #-}
{-# INLINE [2] packByTag #-}


------------------------------------------------------------------------------
-- | Produce only those elements that have their corresponding
--   flag set to `True`.
packByFlag      
        :: Unbox a 
        => Flow FD dist (Bool, a) -> Flow FD BN a
packByFlag ff 
        = packByTag
        $ map (\(b, x) -> (I# (dataToTag# b), x)) ff
{-# INLINE [2] packByFlag #-}


-------------------------------------------------------------------------------
-- | Produce only those elements that match the given predicate.
filter    :: Unbox a 
          => (a -> Bool) -> Flow FD dist a -> Flow FD BN a
filter f ff
        = packByFlag $ map (\x -> (f x, x)) ff
{-# INLINE [2] filter #-}
