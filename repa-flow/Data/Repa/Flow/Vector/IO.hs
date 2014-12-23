
module Data.Repa.Flow.Gang.IO
        ( -- * Sinking Bytes
          fileSinksBytes,      hsSinksBytes)
where
import Data.Repa.Flow.Gang.Base
import Data.Repa.IO.Array
import Data.Repa.Array.Foreign          as R
import Data.Repa.Array                  as R
import Prelude                          as P
import System.IO
import Data.Word


-- | Initialise some file sinks.
fileSinksBytes :: [FilePath] -> IO (Sinks IO (Vector F Word8))
fileSinksBytes filePaths
 = do   hs      <- mapM (flip openBinaryFile WriteMode) filePaths
        hsSinksBytes hs
{-# NOINLINE fileSinksBytes #-}


-- | Write chunks of data to the given file handles.
--
--   TODO: check arity of handles match sinks.
hsSinksBytes :: [Handle] -> IO (Sinks IO (Vector F Word8))
hsSinksBytes hs
 = return $ Sinks (Just $ P.length hs) push_hsSinksBytesF eject_hsSinksBytesF
 where
        push_hsSinksBytesF !ix !chunk
         | ix >= P.length hs
         = error $ "hsSinkBytesF out of range" ++ show (ix, P.length hs)

         | otherwise
         = hPutArray (hs !! ix) chunk
        {-# INLINE push_hsSinksBytesF #-}

        eject_hsSinksBytesF _ 
                = return ()
        {-# INLINE eject_hsSinksBytesF #-}
{-# INLINE [2] hsSinksBytes #-}


