
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
import qualified DDC.Type.Sum   as Sum
import qualified Data.Map       as Map
import Data.Map                 (Map)
import Control.Monad.State.Strict


type DetectS     
        = Map Name GhcName


detectModule :: Module a FatName -> Module a Name
detectModule mm
        = evalState (detect mm) $ Map.empty


-- | Detect flow operators in code converted from GHC Core.
class Detect (c :: * -> *) where
 detect :: c FatName -> State DetectS (c Name)

collect :: Name -> GhcName -> State DetectS ()
collect d g
 = modify $ Map.insert d g


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
        XVar  a u       -> liftM2 XVar     (return a) (detect u)
        XCon  a u       -> liftM2 XCon     (return a) (detect u)
        XLAM  a b x     -> liftM3 XLAM     (return a) (detect b)   (detect x)
        XLam  a b x     -> liftM3 XLam     (return a) (detect b)   (detect x)
        XApp  a x1 x2   -> liftM3 XApp     (return a) (detect x1)  (detect x2)
        XLet  a lts x   -> liftM3 XLet     (return a) (detect lts) (detect x)
        XType t         -> liftM  XType    (detect t)

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

