
module Data.Array.Repa.Vector.Operators.Project
        (Gather(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Flow.Par         as F
import Data.Array.Repa                  as R
import Prelude                          hiding (map)
import GHC.Exts


class Gather r2 a where
 type TG r2

 gatherP :: Source r1 a
         => Vector r1 a
         -> Vector r2 Int
         -> Vector (TG r2) a


instance Gather (O mode dist) a where
 type TG (O mode dist)
        = O mode dist

 gatherP vec (AFlow sh ff arr)
  = let get ix  = unsafeLinearIndex vec (I# ix)
    in  AFlow   sh
                (F.gather get ff)
                (R.map (unsafeLinearIndex vec) arr)

