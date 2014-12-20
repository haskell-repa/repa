
module Data.Repa.IO.Flows
        ( -- * Sinking Bytes
          fileSinksBytesF,      hsSinksBytesF)
where
import Data.Repa.Flows.Internals.Base
import Data.Repa.IO.Array
import Data.Repa.Array.Foreign          as R
import Data.Repa.Array                  as R
import Prelude                          as P
import System.IO
import Data.Word


-- | Initialise some file sinks.
fileSinksBytesF :: [FilePath] -> IO (Sinks (Vector F Word8))
fileSinksBytesF filePaths
 = do   hs      <- mapM (flip openBinaryFile WriteMode) filePaths
        hsSinksBytesF hs
{-# NOINLINE fileSinksBytesF #-}


-- | Write chunks of data to the given file handles.
--
--   TODO: check arity of handles match sinks.
hsSinksBytesF :: [Handle] -> IO (Sinks (Vector F Word8))
hsSinksBytesF hs
 = return $ Sinks (P.length hs) push_hsSinksBytesF eject_hsSinksBytesF
 where
        push_hsSinksBytesF !ix !chunk
                = hPutArrayF (hs !! ix) chunk
        {-# INLINE push_hsSinksBytesF #-}

        eject_hsSinksBytesF _ 
                = return ()
        {-# INLINE eject_hsSinksBytesF #-}
{-# INLINE [2] hsSinksBytesF #-}


