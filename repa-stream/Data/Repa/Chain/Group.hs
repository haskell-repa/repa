{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Group
        (groupsByC)
where
import Data.Repa.Chain.Base
import qualified Data.Vector.Fusion.Stream.Size  as S

#include "vector.h"

-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsByC (==) [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--             = ([3, 2, 1], Just (\'d\', 2))
-- @
--
groupsByC :: (a -> a -> Bool)    -- ^ Comparison function for elements.
         -> Chain a c           -- ^ Input stream of elements.
         -> Chain Int (c, Maybe (a, Int))

groupsByC f (Chain sz  istep s0 iresume)
 = Chain (S.toMax sz) ostep (s0, Nothing) oresume
 where
        oresume (c, m) 
         = (iresume c, m)

        -- Start up by reading the first element, 
        -- either from the continuation or the input stream.
        ostep (si, r@Nothing)
         =  istep si >>= \m 
         -> case m of
                Yield e si'     -> return $ Skip (si', Just (e, 1))
                Skip    si'     -> return $ Skip (si', r)
                Done    di'     -> return $ Done (di', r)

        ostep (si, r@(Just (e1, n)))
         =  istep si >>= \m
         -> case m of
                Yield e2 si'
                 | f e1 e2      -> return $ Skip    (si', Just (e1, n + 1))
                 | otherwise    -> return $ Yield n (si', Just (e2, 1))
                Skip si'        -> return $ Skip    (si', r)
                Done di'        -> return $ Done    (di', r)
        {-# INLINE_INNER ostep #-}

{-# INLINE_STREAM groupsByC #-}
