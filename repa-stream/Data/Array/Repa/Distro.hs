
module Data.Array.Repa.Distro
        (Distro (..))
where
import GHC.Exts
 
-- | Describes the size of a vector, and how the elements are distributed
--   across the gang.
--
--   We have separate 'distroFragLength' and 'distroFragStart' functions
--   for performance reasons, but they must give coherent results, else badness.
--
data Distro
        = Distro
        { -- | Total length of chain.
          distroLength          :: Int#

          -- | Number of fragments the chain is split into.
        , distroFrags           :: Int#

          -- | Get the length of a fragment.
        , distroFragLength      :: Int# -> Int#

          -- | Get where a fragment starts in the result.
        , distroFragStart       :: Int# -> Int# }

