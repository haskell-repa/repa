
module Data.Array.Repa.CoFlow.Seq.Base
        ( CoFlow (..)
        , Snack1 (..)
        , Snack8 (..)
        , uncoflowIO
        , comap)
where
import Data.Array.Repa.CoFlow.Base
import Data.IORef
import GHC.Exts


-------------------------------------------------------------------------------
-- | Feed elements from a vector into a coflow, 
--   but do *NOT* call the eject method afterwards.
--
--   We do not eject the result so that multiple source vectors can be
--   accumulated into the coflow before reading the result.
coflowIO 
        :: Int#                     -- ^ Number of elements to feed in.
        -> (vec a -> Int# -> IO a)  -- ^ Read an element from the vector.
        -> vec a                    -- ^ Read from from this vector.
        -> CoFlow a                 -- ^ Write to this coflow.
        -> IO ()

coflowIO len read vec (CoFlow start eject feed1 feed8)
 = go 0#
 where  
        go8 ix
         | ix      >=# len
         = return ()

         | ix +# 8 >=# len
         = feed1 

         | otherwise
         = feed8 


-- | Extract elements from a coflow into a vector.
-- 
--   We create a new vector using the given functions, and a coflow attached
--   to it. We then call the provided load function to feed elements to our
--   coflow, and return the constructed vector.
uncoflowIO 
        :: (Int#  -> IO (vec a))         -- ^ Create a new vector.
        -> (vec a -> Int# -> a -> IO ()) -- ^ Write an element into the vector.
        -> (CoFlow a -> IO ())           -- ^ Function to load elements into the coflow.
        -> IO (Int, vec a)               -- ^ Completed vector, and number of elts written.

uncoflowIO new write load
 = do   
        let here = "repa-coflow.seq.counflowIO"
        let sIndex      = 0#

        refOut          <- newIORef Nothing

        let start (I# maxLen)
             = do state  <- inew 1
                  iwrite here state sIndex 0#

                  vecOut <- new maxLen
                  return (state, vecOut)

        let eject state
             =    writeIORef refOut (Just state)

        let feed1 (state, vecOut) (Snack1 x)
             = do (I# ix) <- iread here state sIndex
                  write vecOut ix x
                  iwrite here state sIndex (ix +# 1#)
                  return True

        let feed8 (state, vecOut) (Snack8 x0 x1 x2 x3 x4 x5 x6 x7)
             = do (I# ix) <- iread here state sIndex
                  write vecOut (ix +# 0#) x0
                  write vecOut (ix +# 1#) x1
                  write vecOut (ix +# 2#) x2
                  write vecOut (ix +# 3#) x3
                  write vecOut (ix +# 4#) x4
                  write vecOut (ix +# 5#) x5
                  write vecOut (ix +# 6#) x6
                  write vecOut (ix +# 7#) x7
                  iwrite here state sIndex (ix +# 8#)
                  return True

        -- Use the provided function to load data into the coflow.
        load   $ CoFlow start eject feed1 feed8

        -- Read out the completed values.
        Just (state, vec) <- readIORef refOut
        len               <- iread here state sIndex
        return (len, vec)




