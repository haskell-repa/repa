
module Data.Array.Repa.Plugin.ToDDC.Detect.Type where
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Compounds
import Data.Array.Repa.Plugin.FatName
import Data.Array.Repa.Plugin.ToDDC.Detect.Base
import Control.Monad.State.Strict
import qualified DDC.Type.Sum   as Sum
import Data.List


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

         -- Primitive type constructors.
         | Just g'      <- matchPrim "Bool_" n
         -> makePrim g' (NamePrimTyCon PrimTyConBool)           kData

         | Just g'      <- matchPrim "Int_" n
         -> makePrim g' (NamePrimTyCon PrimTyConInt)            kData

         | Just g'      <- matchPrim "Word8_" n
         -> makePrim g' (NamePrimTyCon (PrimTyConWord 8))       kData

         | Just g'      <- matchPrim "Word16_" n
         -> makePrim g' (NamePrimTyCon (PrimTyConWord 16))       kData

         | Just g'      <- matchPrim "Word32_" n
         -> makePrim g' (NamePrimTyCon (PrimTyConWord 32))       kData

         | Just g'      <- matchPrim "Word64_" n
         -> makePrim g' (NamePrimTyCon (PrimTyConWord 64))       kData

         | Just g'      <- matchPrim "Float_" n
         -> makePrim g' (NamePrimTyCon (PrimTyConFloat 32))     kData

         | Just g'      <- matchPrim "Double_" n
         -> makePrim g' (NamePrimTyCon (PrimTyConFloat 64))     kData


         -- Vectors, series and selectors.
         | Just g'      <- matchPrim "Vector_" n
         -> makePrim g' (NameTyConFlow TyConFlowVector)    
                        (kData `kFun` kData)

         | Just g'      <- matchPrim "Series_" n
         -> makePrim g' (NameTyConFlow TyConFlowSeries) 
                        (kRate `kFun` kData `kFun` kData)

         | Just g'      <- matchPrim "Sel1_" n
         -> makePrim g' (NameTyConFlow (TyConFlowSel 1))
                        (kRate `kFun` kRate `kFun` kData)

         -- N-tuples: (,)_ etc. Holds one more than the number of commas
         | Just (str, g')       <- stringPrim n
         , '(':rest             <- str
         , (commas,aftercommas) <- span (==',') rest
         , isPrefixOf ")_" aftercommas
         , size                 <- length commas + 1
         -> do   let k = foldr kFun kData (replicate size kData)
                 makePrim g' (NameTyConFlow (TyConFlowTuple size)) k

         | otherwise
         -> do  collect d g
                return  $ UName d

        UIx ix
         -> return $ UIx ix

        UPrim (FatName g d) t
         -> do  collect d g
                t'      <- detect t
                return  $ UPrim d t'


matchPrim str n
 | Just (str', g)      <- stringPrim n
 , isPrefixOf str str'  = Just g

 | otherwise            = Nothing


stringPrim n
 | FatName g (NameVar str') <- n
 = Just (str', g)

 | FatName g (NameCon str') <- n
 = Just (str', g)

 | otherwise
 = Nothing


makePrim g d t
 = do   collect d g
        return  $ UPrim d t


-- TyCon ----------------------------------------------------------------------
instance Detect TyCon where
 detect tc
  = case tc of
        TyConSort    tc' -> return $ TyConSort tc'
        TyConKind    tc' -> return $ TyConKind tc'
        TyConWitness tc' -> return $ TyConWitness tc'
        TyConSpec    tc' -> return $ TyConSpec tc'

        TyConBound u k
         -> do  u'      <- detect u
                k'      <- detect k
                case u' of
                 UPrim _ k2  -> return $ TyConBound u' k2
                 _           -> return $ TyConBound u' k'



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
        
  -- makePrim g' (NamePrimTyCon (PrimTyConFloat 64))     kData

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

