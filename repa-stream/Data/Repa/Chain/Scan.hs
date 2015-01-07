{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Scan
        ( scanMaybeC
        , groupsByC)
where
import Data.Repa.Chain.Base
import qualified Data.Vector.Fusion.Stream.Size  as S

#include "vector.h"


-------------------------------------------------------------------------------
-- | Perform a left-to-right scan through an input vector, maintaining
--   a state value between each element.
scanMaybeC 
        :: (k -> a -> (k, Maybe b))
        -> k
        -> Chain a  c
        -> Chain b (c, k)

scanMaybeC f k0 (Chain sz istep s0 istart)
 = Chain (S.toMax sz) ostep (s0, k0) ostart
 where
        ostart (c1, k1) 
         = (istart c1, k1)
        {-# INLINE_INNER ostart #-}

        ostep  (c1, k1)
         =  istep c1 >>= \r
         -> case r of
                Yield x c2      
                 -> case f k1 x of
                        (k2, Nothing) -> return $ Skip    (c2, k2)
                        (k2, Just y)  -> return $ Yield y (c2, k2)

                Skip c2               -> return $ Skip    (c2, k1)
                Done c2               -> return $ Done    (c2, k1)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM scanMaybeC #-}


-------------------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) (Just ('a', 4)) 
--                [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--   => ([('a', 7), ('b', 2), ('c', 1)], Just (\'d\', 2))
-- @
--
groupsByC
        :: (a -> a -> Bool)     -- ^ Comparison function.
        -> Maybe (a, Int)       -- ^ Starting element and count.
        -> Chain  a  c          -- ^ Input elements.
        -> Chain (a, Int) 
                 (c, Maybe (a, Int))

groupsByC f !s !vec
 = scanMaybeC work_groupsByC s vec
 where  work_groupsByC !acc !y
         = case acc of
                Nothing      -> (Just (y, 1),     Nothing)
                Just (x, n)
                 | f x y     -> (Just (x, n + 1), Nothing)
                 | otherwise -> (Just (y, 1),     Just (x, n))
        {-# INLINE work_groupsByC #-}
{-# INLINE_STREAM groupsByC #-}

