{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Weave
        ( Move  (..), move
        , Turn  (..)
        , Weave (..)
        , weaveC

        , foldsC)
where
import Data.Repa.Chain.Base
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "vector.h"


-- | What to do after considering two input elements.
data Turn s a
        = Give  a s Move
        | Next    s Move
        | Finish  s


-- | How to move the input chains after considering to input elements.
data Move
        = MoveLeft
        | MoveRight
        | MoveBoth
        | MoveNone


-- | Internal state of a weave.
data Weave s1 a1 s2 a2 k
        = Weave
        { _state1       :: !s1
        , _elem1        :: !(Maybe a1)
        , _state2       :: !s2
        , _elem2        :: !(Maybe a2)
        , _here         :: !k }


-- | Apply a `Move` instruction to a weave state.
move    :: k -> Move 
        -> Weave s1 a1 s2 a2 k -> Weave s1 a1 s2 a2 k
move k' mm ss
 = case mm of
        MoveLeft        -> ss { _here = k', _elem1 = Nothing }
        MoveRight       -> ss { _here = k', _elem2 = Nothing }
        MoveBoth        -> ss { _here = k', _elem1 = Nothing, _elem2 = Nothing }
        MoveNone        -> ss { _here = k' }
{-# INLINE move #-}


-- | Weave two chains.
weaveC  :: Monad m
        => (k -> aL -> aR -> m (Turn k aX))
        -> k
        -> MChain m sL aL -> MChain m sR aR
        -> MChain m (Weave sL aL sR aR k) aX

weaveC f ki (Chain _sz1 s1i step1) (Chain _sz2 s2i step2)
 = Chain S.Unknown 
          (Weave s1i Nothing s2i Nothing ki) 
          step
 where
        step ss@(Weave s1 m1 s2 m2 k)
         = case (m1, m2) of
            (Nothing, _)
             -> step1 s1 >>= \r1
             -> case r1 of
                 Yield x1 s1' -> return $ Skip ss { _state1 = s1', _elem1 = Just x1 }
                 Skip  s1'    -> return $ Skip ss { _state1 = s1' }
                 Done  s1'    -> return $ Done ss { _state1 = s1', _elem1 = Nothing }

            (_, Nothing)
             -> step2 s2 >>= \r2
             -> case r2 of
                 Yield x2 s2' -> return $ Skip ss { _state2 = s2', _elem2 = Just x2 }
                 Skip  s2'    -> return $ Skip ss { _state2 = s2' }
                 Done  s2'    -> return $ Done ss { _state2 = s2', _elem2 = Nothing }

            (Just x1, Just x2)
             -> f k x1 x2 >>= \t
             -> case t of
                 Give x k' m  -> return $ Yield x (move k' m ss)
                 Next   k' m  -> return $ Skip    (move k' m ss)
                 Finish k'    -> return $ Done    (move k' MoveNone ss)
        {-# INLINE step #-}
{-# INLINE_STREAM weaveC #-}


foldsC  :: Monad m
        => (a -> b -> m b)
        -> b
        -> MChain m sLen Int
        -> MChain m sVal a
        -> MChain m (Weave sLen Int sVal a (Maybe Int, b)) b

foldsC f z cLens cVals 
 = weaveC work (Nothing, z) cLens cVals
 where  
        work (mLen, acc) xLen xVal 
         = case mLen of
            Nothing      -> return $ Next (Just xLen, acc) MoveNone
            Just len
             | len == 0  -> return $ Give acc (Nothing, z) MoveBoth
             | otherwise 
             -> f xVal acc >>= \r
             -> return $ Next (mLen, r) MoveRight
        {-# INLINE work #-}
{-# INLINE_STREAM foldsC #-}


