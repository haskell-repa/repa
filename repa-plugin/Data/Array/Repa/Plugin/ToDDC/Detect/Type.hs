
module Data.Array.Repa.Plugin.ToDDC.Detect.Type where
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Compounds
import Data.Array.Repa.Plugin.FatName
import Data.Array.Repa.Plugin.ToDDC.Detect.Base
import Data.Array.Repa.Plugin.ToDDC.Detect.Prim
import Control.Monad.State.Strict
import qualified DDC.Type.Sum   as Sum
import Data.List

import qualified Kind                   as G
import qualified TyCon                  as G
import qualified Var                    as G


-- Bind -----------------------------------------------------------------------
instance Detect Bind where
 detect b
  = case b of
        BName (FatName g d) t1
         -> do  collect d g
                t1'     <- detect t1
                return  $ BName d t1'

        BAnon t -> liftM BAnon (detect t)
        BNone t -> liftM BNone (detect t)


-- Bound ----------------------------------------------------------------------
instance Detect Bound where
 detect u
  = case u of
        -- De-bruijn indices.
        UIx ix
         -> return $ UIx ix

        -- Primitives that we've already detected.
        UPrim (FatName g d) t
         -> do  collect d g
                t'      <- detect t
                return  $ UPrim d t'

        -- Detect a name with a special meaning to us.
        UName n@(FatName g d)
         -- Type constructor names,
         --  we need to check whether the constructor name returns a constraint
         --  kind in case there are type classes in scope that have the same
         --  names as our built-in types -- like 'Vector'
         | Just (str, g') <- stringPrim n
         , Just (dn, ki)  <- detectPrimTyConName str
         , not $ returnsConstraintKind g'
         -> do  collect dn g'
                return  $ UPrim dn ki

         | otherwise
         -> do  collect d g
                return  $ UName d


returnsConstraintKind :: GhcName -> Bool
returnsConstraintKind g
 = case g of
    GhcNameVar   v  -> G.returnsConstraintKind $ G.varType   v
    GhcNameTyCon tc -> G.returnsConstraintKind $ G.tyConKind tc
    _               -> False


-- TyCon ----------------------------------------------------------------------
instance Detect TyCon where
 detect tc
  = case tc of
        TyConSort    tc' -> return $ TyConSort tc'
        TyConKind    tc' -> return $ TyConKind tc'
        TyConWitness tc' -> return $ TyConWitness tc'
        TyConSpec    tc' -> return $ TyConSpec tc'

        -- Detect Unit types,
        --  these look similar to zero arity tuples.
        TyConBound u _
         | UName n        <- u
         , Just (str, _) <- stringPrim n
         , isPrefixOf "()_" str
         -> return $ TyConSpec TcConUnit

        TyConBound u k
         -> do  u'      <- detect u
                k'      <- detect k
                case u' of
                 UPrim _ k2  -> return $ TyConBound u' k2
                 _           -> return $ TyConBound u' k'

        TyConExists ix k   
         -> do  k'      <- detect k
                return  $ TyConExists ix k'


-- Type ------------------------------------------------------------------------
instance Detect Type where
 detect tt

  -- Detect rate variables being applied to Series type constructors.
  | TApp t1 t2  <- tt
  , [ TCon (TyConBound (UName (FatName _ (NameCon str))) _)
    , TVar             (UName (FatName _ n))
    , _]  
                <- takeTApps tt
  , isPrefixOf "Series_" str
  = do  setRateVar n
        t1'     <- detect t1
        t2'     <- detect t2
        return  $ TApp t1' t2'

  -- Detect rate variables being applied to Sel1 type constructors.
  | TApp t1 t2  <- tt
  , [ TCon (TyConBound (UName (FatName _ (NameCon str))) _)
    , TVar             (UName (FatName _ n1))
    , TVar             (UName (FatName _ n2))]  
                <- takeTApps tt
  , isPrefixOf "Sel1_" str
  = do  setRateVar n1
        setRateVar n2
        t1'     <- detect t1
        t2'     <- detect t2
        return  $ TApp t1' t2'


  -- Set kind of detected rate variables to Rate.
  | TForall b t <- tt
  = do  t'      <- detect t
        b'      <- detect b
        case b' of
         BName n _
          -> do rateVar <- isRateVar n
                if rateVar
                 then return $ TForall (BName n kRate) t'
                 else return $ TForall b' t'

         _ -> error "repa-plugin.detect no match"

  -- Convert all kindy things to kData
  | TCon (TyConBound (UName n) _) <- tt
  , Just _       <- matchPrim "*_" n
  = do  return $ TCon (TyConKind KiConData)

  | TCon (TyConBound (UName n) _) <- tt
  , Just _       <- matchPrim "#_" n
  = do  return $ TCon (TyConKind KiConData)

  | TCon (TyConBound (UName n) _) <- tt
  , Just _       <- matchPrim "Constraint_" n
  = do  return $ TCon (TyConKind KiConData)
        
  -- Boilerplate traversal.
  | otherwise
  = case tt of
        TVar u          -> liftM  TVar    (detect u)
        TCon c          -> liftM  TCon    (detect c)
        TForall b t     -> liftM2 TForall (detect b) (detect t)
        TApp t1 t2      -> liftM2 TApp    (detect t1) (detect t2)
        TSum ts         
         -> do  k       <- detect $ Sum.kindOfSum ts
                tss'    <- liftM (Sum.fromList k) $ mapM detect $ Sum.toList ts
                return  $  TSum tss'


-------------------------------------------------------------------------------
-- | If some FatName matches the given string then return the associated GhcName.
matchPrim :: String -> FatName -> Maybe GhcName
matchPrim str n
 | Just (str', g)      <- stringPrim n
 , isPrefixOf str str'  = Just g

 | otherwise            = Nothing


-- | Get the raw String name and the GhcName from a FatName.
stringPrim :: FatName -> Maybe (String, GhcName)
stringPrim n
 | FatName g (NameVar str') <- n
 = Just (str', g)

 | FatName g (NameCon str') <- n
 = Just (str', g)

 | otherwise
 = Nothing

