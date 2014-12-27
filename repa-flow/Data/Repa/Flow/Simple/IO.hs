
module Data.Repa.Flow.Simple.IO
        ( -- * Sourcing Bytes
          fileSourceBytes,     hSourceBytes

          -- * Sourcing Records
        , fileSourceRecords,   hSourceRecords

          -- * Sinking Bytes
        , fileSinkBytes,       hSinkBytes)
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.IO.Array
import Data.Repa.Array.Foreign          as R
import Data.Repa.Array                  as R
import System.IO
import Data.Word




-- Sink -------------------------------------------------------------------------------------------

