
module Data.Repa.Stream.Insert
        (insertS)
where
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Insert elements produced by the given function in to a stream.
insertS  :: Monad m
        => (Int -> Maybe a)     -- ^ Produce a new element for this index.
        -> Stream m a           -- ^ Source stream.
        -> Stream m a

insertS fNew (Stream istep si0 _)
 = Stream ostep (si0, 0, True, False) S.Unknown
  where
        ostep (si, ix, tryNew, srcDone)
         | tryNew
         = case fNew ix of
                Just x          -> return $ Yield x (si,  ix + 1, True,  srcDone)
                Nothing
                 | srcDone      -> return Done
                 | otherwise    -> ostep_next    si ix

         | otherwise
         = ostep_next si ix
        {-# INLINE_INNER ostep #-}

        ostep_next !si !ix
         =  istep si >>= \m
         -> case m of
                Yield x si'     -> return $ Yield x (si', ix + 1, True,  False)
                Skip    si'     -> return $ Skip    (si', ix,     False, False)
                Done            -> return $ Skip    (si,  ix,     True,  True)
        {-# INLINE_INNER ostep_next #-}
{-# INLINE_STREAM insertS #-}
