module Data.Array.Repa.Bulk.Base
        ( vnew
        , vwrite
        , vfreeze)
where
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as VM


vnew    = VM.unsafeNew
vwrite  = VM.unsafeWrite
vfreeze = V.unsafeFreeze
