
module Data.Array.Repa.Flow.Par.Base
        ( Flow   (..)
        , Distro (..)
        , Unflow (..))
where
import Data.Array.Repa.Flow.Par.Distro
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import qualified Data.Array.Repa.Flow.Seq       as Seq
import qualified Data.Array.Repa.Flow.Seq.Base  as Seq
import qualified Data.Array.Repa.Eval.Gang      as Gang
import System.IO.Unsafe
import GHC.Exts


data Flow r d a
        = Flow
        { -- | Flow distribution, keeps track of how well-balanced
          --   our workload is between the threads.
          flowDistro    :: Distro d

          -- | Get the flow fragment that runs on the given thread.
        , flowFrag      :: Int# -> Seq.Flow r a }

        
-------------------------------------------------------------------------------
class Unflow d where
 unflow :: (Seq.Touch a, U.Unbox a) => Flow Seq.FD d a -> U.Vector a


instance Unflow L where
 unflow !ff
  = unsafePerformIO
  $ do  let !distro   = flowDistro ff

        !mvec   <- UM.unsafeNew 
                $  I# (distroBalancedLength distro)

        let !getStart = distroBalancedFragStart distro

        -- The action that runs on each thread.
        let action (I# tid)
             = do let !ixStart    = getStart tid

                  let write (I# ix)  
                        = UM.unsafeWrite mvec (I# (ixStart +# ix))

                  case flowFrag ff tid of
                   Seq.Flow start _size _report get1 get8
                    -> do state <- start
                          Seq.slurp ixStart Nothing write
                                (get1 state) (get8 state)
                          return ()

        Gang.gangIO Gang.theGang action

        U.unsafeFreeze mvec

