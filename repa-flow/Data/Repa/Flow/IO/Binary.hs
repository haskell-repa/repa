
module Data.Repa.Flow.IO.Binary
        ( sourceBinary )
where
import Data.Repa.Array.Material         as A
import Data.Repa.Array.Generic          as A
import Data.Repa.Flow.IO.Storable       as F
import Data.Repa.Flow.IO.Bucket         as F
import Data.Repa.Flow.Generic           as G
#include "repa-flow.h"


-- Move to Data.Repa.Flow.Binary
sourceBinary 
        :: F.Storable a
        => Spec a                       -- ^ Specification of elements.
        -> Integer                      -- ^ Number of elements per chunk.
        -> Array B Bucket               -- ^ Buckets of table.
        -> IO (G.Sources Int IO (Array (Rep a) a))

sourceBinary spec lenElems bs
 = return $ G.Sources (A.length bs) pull_sourceBinary
 where
        pull_sourceBinary i eat eject
         = do   let b   = A.index bs i
                op      <- bIsOpen b
                if not op
                 then eject
                 else do
                  eof   <- bAtEnd b
                  if eof
                   then eject
                   else do
                        Just !chunk <- getArray spec lenElems b
                        eat chunk
        {-# INLINE pull_sourceBinary #-}
{-# INLINE_FLOW sourceBinary #-}

