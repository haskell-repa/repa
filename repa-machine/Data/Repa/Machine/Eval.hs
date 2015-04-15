
module Data.Repa.Machine.Eval
        (eval)
where
import Data.Repa.Machine.Step
import Data.Repa.Machine.Base
import Control.Monad.ST
import Data.STRef
import Data.Repa.Query.Eval.Env                 (Env)
import qualified Data.Repa.Query.Eval.Env       as Env
import qualified Data.Repa.Query.Exp            as Exp


eval    :: ( Ord label, Env env [Lit]
           , Ord (Env.Bound env)
           , Show label, Show (Env.Bound env))
        => env [Lit]            -- ^ Data for input streams.
        -> env [Lit]            -- ^ Starting data for output streams.
        -> Machine label env    -- ^ Machine to run.
        -> env [Lit]            -- ^ Output values for each stream.

eval ins0 outs0 machine
 = runST
 $ do   
        refIns     <- newSTRef ins0
        refOuts    <- newSTRef outs0

        -- Get a value from one of the input streams.
        let get u
             = do ins   <- readSTRef refIns
                  case Env.lookup u ins of
                   Nothing  
                    -> return Nothing

                   Just (l : ls)
                    -> do writeSTRef refIns (Env.update u ls ins)
                          return $ Just $ Exp.VLit () l

                   Just []
                    ->    return Nothing

        -- Put a value to one of the output streams.
        let put u (Just (Exp.VLit _ o))
             = do outs <- readSTRef refOuts
                  case Env.lookup u outs of
                   Nothing -> return ()
                   Just os
                    -> do writeSTRef refOuts (Env.update u (os ++ [o]) outs)
                          return ()

            put _ (Just Exp.VLam{})
             = error "eval: not putting lambda"

            put _ Nothing
             = return ()

        -- Keep stepping the machine until it throws
        -- an error or halts.
        let go !m
             = do
                  result    <- step get put m
                  case result of
                   Left  err         -> error $ show err
                   Right m'     
                    | machineHalt m' -> return ()
                    | otherwise      -> go m'

        go machine

        -- Return data in output streams.
        readSTRef refOuts

