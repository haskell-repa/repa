
module Data.Array.Repa.Plugin.ToDDC.Detect
        (detectModule)
where
import Data.Array.Repa.Plugin.FatName
import Data.Array.Repa.Plugin.ToDDC.Detect.Base
import Data.Array.Repa.Plugin.ToDDC.Detect.Type  ()

import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds

import Control.Monad.State.Strict

import qualified Data.Map       as Map
import Data.Map                 (Map)
import Data.List


detectModule 
        :: Module a FatName 
        -> (Module a Name, Map Name GhcName)

detectModule mm
 = let  (mm', state')    = runState (detect mm) $ zeroState
   in   (mm', stateNames state')



-- Module ---------------------------------------------------------------------
instance Detect (Module a) where
 detect mm
  = do  body'   <- detect     (moduleBody mm)
        importK <- detectMap  (moduleImportKinds mm)
        importT <- detectMap  (moduleImportTypes mm)
        return  $ ModuleCore
                { moduleName            = moduleName mm
                , moduleExportKinds     = Map.empty
                , moduleExportTypes     = Map.empty
                , moduleImportKinds     = importK
                , moduleImportTypes     = importT
                , moduleBody            = body' }

-- Convert the FatNames of an import map
detectMap  :: Map FatName (QualName FatName, Type FatName)
           -> State DetectS (Map Name (QualName Name, Type Name))
detectMap  m
 = do   let ms = Map.toList   m
        ms'   <- mapM detect' ms
        return $ Map.fromList ms'
 where
  detect' (FatName _ k,(QualName mn (FatName _ n), t))
   = do t' <- detect t
        return (k, (QualName mn n, t'))


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

        -- Booleans
        DaConNamed (FatName g d@(NameCon v))
         | isPrefixOf "True_" v
         -> do  collect d g
                return $ DaConNamed (NameLitBool True)
        DaConNamed (FatName g d@(NameCon v))
         | isPrefixOf "False_" v
         -> do  collect d g
                return $ DaConNamed (NameLitBool False)

        DaConNamed (FatName g d)
         -> do  collect d g
                return $ DaConNamed d


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

         _ -> error "repa-plugin.detect[Exp] no match"

  -- Detect vectorOfSeries
  | XApp a _ _                           <- xx
  , Just  (XVar _ u,     [xTK, xTA, _xD, xS]) 
                                        <- takeXApps xx
  , UName (FatName _ (NameVar v))        <- u
  , isPrefixOf "toVector_" v
  = do  args'   <- mapM detect [xTK, xTA, xS]
        return  $ xApps a (XVar a (UPrim (NameOpFlow OpFlowVectorOfSeries)
                                         (typeOpFlow OpFlowVectorOfSeries)))
                          args'

  -- Detect folds.
  | XApp a _ _                          <- xx
  , Just  (XVar _ uFold, [xTK, xTA, xTB, _xD, xF, xZ, xS])    
                                        <- takeXApps xx
  , UName (FatName _ (NameVar vFold))   <- uFold
  , isPrefixOf "fold_" vFold
  = do  args'   <- mapM detect [xTK, xTA, xTB, xF, xZ, xS]
        return  $  xApps a (XVar a (UPrim (NameOpFlow OpFlowFold) 
                                          (typeOpFlow OpFlowFold)))
                           args'

  -- Detect maps
  | XApp a _ _                          <- xx
  , Just  (XVar _ uMap,  [xTK, xTA, xTB, _xD1, _xD2, xF, xS ])
                                        <- takeXApps xx
  , UName (FatName _ (NameVar vMap))    <- uMap
  , isPrefixOf "map_" vMap
  = do  args'   <- mapM detect [xTK, xTA, xTB, xF, xS]
        return  $ xApps a (XVar a (UPrim (NameOpFlow (OpFlowMap 1))
                                         (typeOpFlow (OpFlowMap 1))))
                          args'

  -- Detect packs
  | XApp a _ _                          <- xx
  , Just  (XVar _ uPack,  [xTK1, xTK2, xTA, _xD1, xSel, xF])
                                        <- takeXApps xx
  , UName (FatName _ (NameVar vPack))   <- uPack
  , isPrefixOf "pack_" vPack
  = do  args'   <- mapM detect [xTK1, xTK2, xTA, xSel, xF]
        return  $ xApps a (XVar a (UPrim (NameOpFlow OpFlowPack)
                                         (typeOpFlow OpFlowPack)))
                          args'

  -- Detect mkSels
  | XApp a _ _                          <- xx
  , Just  (XVar _ u,    [xTK, xTA, xFlags, xWorker])
                                        <- takeXApps xx
  , UName (FatName _ (NameVar v))       <- u
  , isPrefixOf "mkSel1_" v
  = do  args'   <- mapM detect [xTK, xTA, xFlags, xWorker]
        return  $ xApps a (XVar a (UPrim (NameOpFlow (OpFlowMkSel 1))
                                         (typeOpFlow (OpFlowMkSel 1))))
                          args'

  -- Detect n-tuples
  | XApp a _ _                          <- xx
  , Just  (XVar _ uTuple,  args)        <- takeXApps xx
  , UName (FatName _ (NameVar vTuple))  <- uTuple

  , size                                <- length args `div` 2
  , commas                              <- replicate (size-1) ','
  , prefix                              <- "(" ++ commas ++ ")_"

  , size > 1
  , isPrefixOf prefix vTuple
  = do  args'   <- mapM detect args
        let tuple = DaConFlowTuple size
            ty    = typeDaConFlow tuple
        return  $ xApps a (XCon a $ mkDaConAlg (NameDaConFlow tuple) ty)
                          args'



  -- Inject type arguments for arithmetic ops.
  --   In the Core code, arithmetic operations are expressed as monomorphic
  --   dictionary methods, which we convert to polytypic DDC primops.
  | XVar a (UName (FatName nG (NameVar str)))    <- xx
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

        XCase a x alts  -> liftM3 XCase (return a) (detect x)   (mapM detect alts)
        XCast{}         -> error "repa-plugin.detect: XCast not handled"
        XWitness{}      -> error "repa-plugin.detect: XWitness not handled"


-- Match arithmetic operators.
matchPrimArith :: String -> Maybe (Name, Type Name, Type Name)
matchPrimArith str
 -- Num
 | isPrefixOf "$fNumInt_$c+_" str       
 = Just (NamePrimArith PrimArithAdd, tInt, typePrimArith PrimArithAdd)

 | isPrefixOf "$fNumInt_$c-_" str       
 = Just (NamePrimArith PrimArithSub, tInt, typePrimArith PrimArithSub)

 | isPrefixOf "$fNumInt_$c*_" str
 = Just (NamePrimArith PrimArithMul, tInt, typePrimArith PrimArithMul)

 -- Integral
 | isPrefixOf "$fIntegralInt_$cdiv_" str
 = Just (NamePrimArith PrimArithDiv, tInt, typePrimArith PrimArithDiv)

 | isPrefixOf "$fIntegralInt_$crem_" str
 = Just (NamePrimArith PrimArithRem, tInt, typePrimArith PrimArithRem)

 | isPrefixOf "$fIntegralInt_$cmod_" str
 = Just (NamePrimArith PrimArithMod, tInt, typePrimArith PrimArithMod)

 -- Eq
 | isPrefixOf "eqInt_" str
 = Just (NamePrimArith PrimArithEq,  tInt, typePrimArith PrimArithEq)

 | isPrefixOf "gtInt_" str
 = Just (NamePrimArith PrimArithGt,  tInt, typePrimArith PrimArithGt)

 | isPrefixOf "ltInt_" str
 = Just (NamePrimArith PrimArithLt,  tInt, typePrimArith PrimArithLt)

 | otherwise
 = Nothing


--- Lets ----------------------------------------------------------------------
instance Detect (Lets a) where
 detect ll
  = case ll of
        LLet _ b x      
         -> do  b'      <- detect b
                x'      <- detect x
                return  $ LLet LetStrict b' x'

        LRec bxs        
         -> do  let (bs, xs) = unzip bxs
                bs'     <- mapM detect bs
                xs'     <- mapM detect xs
                return  $ LRec $ zip bs' xs'

        LLetRegions{}   -> error "repa-plugin.detect: LLetRegions not handled"
        LWithRegion{}   -> error "repa-plugin.detect: LWithRegions not handled"


--- Alt  ----------------------------------------------------------------------
instance Detect (Alt a) where
 detect (AAlt p x)
  = liftM2 AAlt (detect p) (detect x)

instance Detect Pat where
 detect p
  = case p of
        PDefault
         -> return PDefault
        PData dc bs
         -> liftM2 PData (detect dc) (mapM detect bs)

