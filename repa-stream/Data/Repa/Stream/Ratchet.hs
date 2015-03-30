
module Data.Repa.Stream.Ratchet
        ( unsafeRatchetS)
where
import Control.Monad.Primitive
import Data.IORef
import Data.Vector.Fusion.Stream.Monadic         (Stream(..), Step(..))
import qualified Data.Vector.Generic             as GV
import qualified Data.Vector.Generic.Mutable     as GM
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Interleaved `enumFromTo`. 
--
--   Given a vector of starting values, and a vector of stopping values, 
--   produce an stream of elements where we increase each of the starting
--   values to the stopping values in a round-robin order. Also produce a
--   vector of result segment lengths.
--
-- @
--  unsafeRatchetS [10,20,30,40] [15,26,33,47]
--  =  [10,20,30,40       -- 4
--     ,11,21,31,41       -- 4
--     ,12,22,32,42       -- 4
--     ,13,23   ,43       -- 3
--     ,14,24   ,44       -- 3
--        ,25   ,45       -- 2
--              ,46]      -- 1
--
--         ^^^^             ^^^
--       Elements         Lengths
-- @
--
--   The function takes the starting values in a mutable vector and 
--   updates it during computation. Computation proceeds by making passes
--   through the mutable vector and updating the starting values until
--   they match the stopping values. 
--
--   UNSAFE: Both input vectors must have the same length, 
--           but this is not checked.
--
unsafeRatchetS 
        :: (GM.MVector vm Int, GV.Vector vv Int)
        => vm (PrimState IO) Int         -- ^ Starting values. Overwritten duing computation.
        -> vv Int                        -- ^ Ending values
        -> IORef (vm (PrimState IO) Int) -- ^ Vector holding segment lengths.
        -> Stream IO   Int

unsafeRatchetS !mvStarts !vMax !rmvLens
 = Stream ostep (0, Nothing, 0, 0) S.Unknown
 where
        !iSegMax = GM.length mvStarts - 1

        ostep (iSeg, mvmLens, oSeg, oLen)
         = ostep' iSeg mvmLens oSeg oLen
        {-# INLINE ostep #-}

        ostep' !iSeg !mvmLens !oSeg !oLen
         | iSeg <= iSegMax
         = do   !iVal      <- GM.unsafeRead mvStarts iSeg
                let !iNext = vMax `GV.unsafeIndex` iSeg
                if  iVal >= iNext
                 then   return $ Skip       (iSeg + 1, mvmLens, oSeg, oLen)
                 else do
                        GM.unsafeWrite mvStarts iSeg (iVal + 1)
                        return $ Yield iVal (iSeg + 1, mvmLens, oSeg, oLen + 1)

         -- We're at the end of an output segment, 
         -- so write the output length into the lengths vector.
         | oLen > 0
         = do   -- Get the current output vector.
                !vmLens  <- case mvmLens of
                              Nothing     -> readIORef rmvLens
                              Just vmLens -> return $ vmLens

                -- If the output vector is full then we need to grow it.
                let !oSegLen = GM.length vmLens
                if   oSeg >= oSegLen
                 then do
                        !vmLens' <- GM.unsafeGrow vmLens (GM.length vmLens)
                        writeIORef rmvLens vmLens'
                        GM.unsafeWrite vmLens' oSeg oLen
                        return $ Skip (0, Just vmLens', oSeg + 1, 0)

                 else do
                        GM.unsafeWrite vmLens  oSeg oLen
                        return $ Skip (0, Just vmLens,  oSeg + 1, 0)

         | otherwise
         = do   !vmLens  <- case mvmLens of
                                Nothing     -> readIORef rmvLens
                                Just vmLens -> return $ vmLens

                let !vmLens' = GM.unsafeSlice 0 oSeg vmLens
                writeIORef rmvLens vmLens'
                return Done
        {-# INLINE_INNER ostep' #-}
{-# INLINE_STREAM unsafeRatchetS #-}


