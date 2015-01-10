{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Weave
        ( weaveC
        , Weave (..)
        , Turn  (..)
        , Move  (..), move)
where
import Data.Repa.Chain.Base
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "vector.h"


-- | A weave is a generalized merge of two input chains.
--
--   The worker function takes the current state, values from the 
--   left and right input chains, and produces a `Turn` which 
--   describes any output at that point, as well as how the input
--   chains should be advanced.
--
weaveC  :: Monad m
        => (k -> aL -> aR -> m (Turn k aX))     -- ^ Worker function.
        -> k                                    -- ^ Initial state.
        -> Chain m sL aL                        -- ^ Left input chain.
        -> Chain m sR aR                        -- ^ Right input chain.
        -> Chain m (Weave sL aL sR aR k) aX     -- ^ Result chain.

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
                 Yield x1 sL' -> return $ Skip ss { _stateL = sL', _elemL = Just x1 }
                 Skip  sL'    -> return $ Skip ss { _stateL = sL' }
                 Done  sL'    -> return $ Done ss { _stateL = sL', _elemL = Nothing }

            (_, Nothing)
             -> step2 s2 >>= \r2
             -> case r2 of
                 Yield x2 sR' -> return $ Skip ss { _stateR = sR', _elemR = Just x2 }
                 Skip  sR'    -> return $ Skip ss { _stateR = sR' }
                 Done  sR'    -> return $ Done ss { _stateR = sR', _elemR = Nothing }

            (Just x1, Just x2)
             -> f k x1 x2 >>= \t
             -> case t of
                 Give x k' m  -> return $ Yield x (move k' m ss)
                 Next   k' m  -> return $ Skip    (move k' m ss)
                 Finish k'    -> return $ Done    (move k' MoveNone ss)
        {-# INLINE step #-}
{-# INLINE_STREAM weaveC #-}


-- | Internal state of a weave.
data Weave sL aL sR aR k
        = Weave
        { -- | State of the left input chain.
          _stateL       :: !sL

          -- | Current value loaded from the left input.
        , _elemL        :: !(Maybe aL)

          -- | State of the right input chain.
        , _stateR       :: !sR

          -- | Current value loaded from the right input.
        , _elemR        :: !(Maybe aR)

          -- | Worker state at this point in the weave.
        , _here         :: !k }
        deriving Show


-- | What to do after considering two input elements.
data Turn k a
        = Give  !a !k !Move     -- ^ Give an element and a new state.
        | Next     !k !Move     -- ^ Move to the next input.
        | Finish   !k           -- ^ Weave is finished for now.
        deriving Show


-- | How to move the input chains after considering to input elements.
data Move
        = MoveLeft
        | MoveRight
        | MoveBoth
        | MoveNone
        deriving Show


-- | Apply a `Move` instruction to a weave state.
move    :: k -> Move 
        -> Weave s1 a1 s2 a2 k -> Weave s1 a1 s2 a2 k
move k' mm ss
 = case mm of
        MoveLeft        -> ss { _here = k', _elemL = Nothing }
        MoveRight       -> ss { _here = k', _elemR = Nothing }
        MoveBoth        -> ss { _here = k', _elemL = Nothing, _elemR = Nothing }
        MoveNone        -> ss { _here = k' }
{-# INLINE move #-}

