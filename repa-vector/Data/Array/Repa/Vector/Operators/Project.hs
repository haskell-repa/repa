
module Data.Array.Repa.Vector.Operators.Project
        (Gather(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Flow.Par         as F
import Prelude                          hiding (map)
import GHC.Exts


class Gather r a where
 type TG r

 gather  :: Bulk r2 a
         => Vector r2 a
         -> Vector r Int
         -> Vector (TG r) a


instance Gather (O mode dist) a where
 type TG (O mode dist)
        = O mode dist

 gather vec (AFlow ff)
  = let get ix  = linearIndex vec (I# ix)
    in  AFlow   (F.gather get ff)

