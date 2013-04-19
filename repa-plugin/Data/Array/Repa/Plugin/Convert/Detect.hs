
module Data.Array.Repa.Plugin.Convert.Detect
        (detectModule)
where
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Name
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Env
import Data.Array.Repa.Plugin.Convert.FatName
import Data.List
import Data.Map                 (Map)
import Data.Set                 (Set)
import Control.Monad.State.Strict

import qualified DDC.Type.Sum   as Sum
import qualified Data.Map       as Map
import qualified Data.Set       as Set


detectModule :: Module a FatName -> Module a Name
detectModule mm
        = evalState (detect mm) $ zeroState


-- | Detect flow operators in code converted from GHC Core, rewriting the raw
--   AST converted from GHC to be a well formed Disciple Core program. 
--   After this pass the code should type check.
--
class Detect (c :: * -> *) where
 detect :: c FatName -> State DetectS (c Name)


-- Detect State ---------------------------------------------------------------
data DetectS     
        = DetectS
        { -- Map of Disciple Core names to GHC Core Names.
          stateNames    :: Map Name GhcName

          -- Names of rate variables, which we discover as they are arguments
          -- of Stream type constructors. 
          -- In GHC core rate variables have kind '*', 
          --   but for Disciple Core we change them to have kind 'Rate'.
        , stateRateVars :: Set Name }


zeroState :: DetectS
zeroState
        = DetectS
        { stateNames    = Map.empty
        , stateRateVars = Set.empty }


collect :: Name -> GhcName -> State DetectS ()
collect !d !g
 = modify $ \s -> s { stateNames    = Map.insert d g (stateNames s) }


setRateVar :: Name -> State DetectS ()
setRateVar !name
 = modify $ \s -> s { stateRateVars = Set.insert name (stateRateVars s) }


isRateVar  :: Name -> State DetectS Bool
isRateVar name
 = do   s       <- gets stateRateVars 
        return  $ Set.member name s


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
        UName n@(FatName g d)
         -- Data Constructors.
         | Just g       <- matchPrim "Int_" n
         -> makePrim g (NamePrimTyCon   PrimTyConInt)    
                       kData

         | Just g       <- matchPrim "Stream_" n
         -> makePrim g (NameDataTyCon   DataTyConStream) 
                       (kData `kFun` kData `kFun` kData)

         | otherwise
         -> do  collect d g
                return  $ UName d

        UIx ix
         -> return $ UIx ix

        UPrim n@(FatName g d) t
         -> do  collect d g
                t'      <- detect t
                return  $ UPrim d t'

matchPrim str n
 | FatName g (NameVar str') <- n
 , isPrefixOf str str'  = Just g

 | FatName g (NameCon str') <- n
 , isPrefixOf str str'  = Just g

 | otherwise            = Nothing

makePrim g d t
 = do   collect d g
        return  $ UPrim d t



-- DaCon ----------------------------------------------------------------------
instance Detect DaCon where
 detect (DaCon dcn t isAlg)
  = do  dcn'    <- detect dcn
        t'      <- detect t
        return  $  DaCon dcn' t' isAlg


instance Detect DaConName where
 detect dcn
  = case dcn of
        DaConUnit       
         -> return DaConUnit

        DaConNamed (FatName g d)
         -> do  collect d g
                return $ DaConNamed d


-- TyCon ----------------------------------------------------------------------
instance Detect TyCon where
 detect tc
  = case tc of
        TyConSort  tc   -> return $ TyConSort tc
        TyConKind  tc   -> return $ TyConKind tc
        TyConWitness tc -> return $ TyConWitness tc
        TyConSpec  tc   -> return $ TyConSpec tc
        TyConBound u k  -> liftM2 TyConBound (detect u) (detect k)


-- Type ------------------------------------------------------------------------
instance Detect Type where
 detect tt

  -- Detect rate variables being applied to Stream type constructors.
  | TApp t1 t2  <- tt
  , [ TCon (TyConBound (UName (FatName _ (NameCon str))) _)
    , TVar             (UName (FatName _ n))
    , _]  
                <- takeTApps tt
  , isPrefixOf "Stream_" str
  = do  setRateVar n
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


-- Module ---------------------------------------------------------------------
instance Detect (Module a) where
 detect mm
  = do  body'   <- detect (moduleBody mm)
        return  $ ModuleCore
                { moduleName            = moduleName mm
                , moduleExportKinds     = Map.empty
                , moduleExportTypes     = Map.empty
                , moduleImportKinds     = Map.empty
                , moduleImportTypes     = Map.empty
                , moduleBody            = body' }


-- Match arithmetic operators.
matchPrimArith :: String -> Maybe (Name, Type Name, Type Name)
matchPrimArith str
 | isPrefixOf "$fNumInt_$c+_" str       
 = Just (NamePrimArith PrimArithAdd, tIntU, typeOfPrimArith PrimArithAdd)

 | isPrefixOf "$fNumInt_$c*_" str
 = Just (NamePrimArith PrimArithMul, tIntU, typeOfPrimArith PrimArithMul)

 | otherwise
 = Nothing


-- Exp ------------------------------------------------------------------------
instance Detect (Exp a) where
 detect xx

  -- Set kind of detected rate variables to Rate.
  | XLam a b x          <- xx
  = do  b'      <- detect b
        x'      <- detect x
        case b' of
         BName n _
          -> do rateVar <- isRateVar n
                if rateVar 
                 then return $ XLAM a (BName n kRate) x'
                 else return $ XLam a b' x'


  -- Detect folds.
  | XApp a _ _                            <- xx
  , Just (XVar _ uFold, [xTK, xTA, xTB, xF, xZ, xS])
                                          <- takeXApps xx
  , UName (FatName nG nD@(NameVar vFold)) <- uFold
  , isPrefixOf "fold_" vFold
  = detect  $ xApps a (XVar a (UName (FatName nG (NameFlowOp FlowOpFold))))
                      [xTK, xTA, xTB, xF, xZ, xS]


  -- Inject required type arguments for arithmetic ops
  | XVar a (UName (FatName nG nD@(NameVar str)))    <- xx
  , Just (nD', tArg, tPrim)  <- matchPrimArith str
  = do  collect nD' nG
        return  $ xApps a (XVar a (UPrim nD' tPrim)) [XType tArg]


  -- Strip boxing constructors from literal values.
  | XApp _ (XVar _ (UName (FatName _ (NameCon str1)))) x2 <- xx
  , isPrefixOf "I#_" str1
  = detect x2
  
  -- Boilerplate traversal.
  | otherwise
  = case xx of
        XVar  a u       -> liftM2 XVar  (return a) (detect u)
        XCon  a u       -> liftM2 XCon  (return a) (detect u)
        XLAM  a b x     -> liftM3 XLAM  (return a) (detect b)   (detect x)
        XLam  a b x     -> liftM3 XLam  (return a) (detect b)   (detect x)
        XApp  a x1 x2   -> liftM3 XApp  (return a) (detect x1)  (detect x2)
        XLet  a lts x   -> liftM3 XLet  (return a) (detect lts) (detect x)
        XType t         -> liftM  XType (detect t)

        XCase a x alts  -> error "repa-plugin.detect: XCase not handled"
        XCast a c x     -> error "repa-plugin.detect: XCast not handled"
        XWitness w      -> error "repa-plugin.detect: XWitness not handled"


instance Detect (Lets a) where
 detect ll
  = case ll of
        LLet m b x      
         -> do  b'      <- detect b
                x'      <- detect x
                return  $ LLet LetStrict b' x'

        LRec bxs        
         -> do  let (bs, xs) = unzip bxs
                bs'     <- mapM detect bs
                xs'     <- mapM detect xs
                return  $ LRec $ zip bs' xs'

        LLetRegions{}   -> error "repa-plugin.detect: LLetRegions not handled"
        LWithRegion{}   -> error "repa-plugin.detect: LWitnRegions not handled"

