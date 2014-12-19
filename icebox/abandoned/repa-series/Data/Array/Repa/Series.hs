module Data.Array.Repa.Series
        ( -- * Rates
          RateNat (..)
        , rateNatOfInt

        , Down4   (..)
        , Tail4   (..)

          -- * Series
        , Series  (..)

          -- * Vectors
        , Vector (..)
        , fromPrimitive
        , toPrimitive

          -- * Selectors
        , Sel1   (..)
        , mkSel1


          -- * Processes
        , Process       (..)
        , pjoin, (%)
        , runProcess
        , runProcess2
        , runProcess3
        , runProcess4

          -- * Series combinators
        , map
        , map2
        , pack

          -- * Process constructors
        , fill
        , reduce

          -- * Primitives used by the Repa plugin
        , Primitives (..)
        , primitives)
where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Series
import Data.Array.Repa.Series.Sel
import Data.Array.Repa.Series.Vector
import Data.Array.Repa.Series.Fallback
import Data.Array.Repa.Series.Prim
import Data.Array.Repa.Series.Process
import Prelude hiding (map)

import Data.Primitive.Types
import GHC.Exts


instance Prim Bool where
 sizeOf# _              = 1#
 alignment# _           = 1#

 indexByteArray# arr ix 
  = isTrue# (word2Int# (indexWord8Array# arr ix))

 readByteArray# marr ix world
  = case readWord8Array# marr ix world of
        (# world, word #) -> (# world, isTrue# (word2Int# word) #)

 writeByteArray# marr ix x world
  = writeWord8Array# marr ix (int2Word# (dataToTag# x)) world

 setByteArray# marr start len val world
  = GHC.Exts.setByteArray# marr start len (dataToTag# val) world

 indexOffAddr# addr ix 
  = isTrue# (word2Int# (indexWord8OffAddr# addr ix))

 readOffAddr#  addr off world
  = case readWord8OffAddr# addr off world of
        (# world, word #) -> (# world, isTrue# (word2Int# word) #)

 writeOffAddr# addr off x world
  = writeWord8OffAddr# addr off (int2Word# (dataToTag# x)) world

 setOffAddr#   marr start len val world
  = error "setOffAddr[Bool] not implemented"
