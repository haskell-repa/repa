
module Data.Array.Repa.Plugin.Convert.ToGHC.Env
        ( Env (..)
        , bindVarX
        , bindVarT)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.ToGHC.Var
import Data.Array.Repa.Plugin.Convert.FatName
import Data.Map                         (Map)

import qualified HscTypes                as G
import qualified Type                    as G
import qualified UniqSupply              as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Flow           as D



-- | Environment used to map DDC names to GHC names.
--   Used when converting DDC Core to GHC core.
data Env
        = Env 
        { -- | Guts of the original GHC module.
          envGuts       :: G.ModGuts

          -- | Name map we got during the original GHC -> DDC conversion.
        , envNames      :: Map D.Name GhcName

          -- | Locally scoped type variables.
        , envVarsT      :: [(D.Name, G.Var)]

          -- | Locall scoped expression variables.
        , envVarsX      :: [(D.Name, G.Var)]
        }


-- | Bind a fresh GHC variable for a DDC expression variable.
bindVarX :: Env -> D.Bind D.Name -> G.UniqSM (Env, G.Var)
bindVarX env (D.BName n@(D.NameVar str) t)
 = do   let gt   = convertType (envNames env) t
        gv       <- newDummyVar str gt
        let env' = env { envVarsX       = (n, gv) : envVarsX env }
        return   (env', gv)

bindVarX env (D.BNone t)
 = do   let gt   = convertType (envNames env) t
        gv       <- newDummyVar "x" gt
        return  (env, gv)

bindVarX _ b
        = error $ "repa-plugin.ToGHC.bindVarX: can't bind " ++ show b


-- | Bind a fresh GHC variable for a DDC expression variable.
bindVarT :: Env -> D.Bind D.Name -> G.UniqSM (Env, G.Var)
bindVarT env (D.BName n@(D.NameVar str) t)
 = do   let gt   = convertType (envNames env) t
        gv       <- newDummyVar str gt
        let env' = env { envVarsT       = (n, gv) : envVarsT env }
        return   (env', gv)

bindVarT env (D.BNone t)
 = do   let gt   = convertType (envNames env) t
        gv       <- newDummyVar "t" gt
        return  (env, gv)

bindVarT _ b
        = error $ "repa-plugin.ToGHC.bindVarT: can't bind " ++ show b

