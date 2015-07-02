{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.Repa.Query.Build.Repa.Exp
        ( expOfExp
        , expOfScalarOp
        , expOfLit
        , expOfMask
        , expOfDelimFields
        , expOfDelim
        , expOfFieldsFormat
        , expOfFieldFormat
        , expOfNat)
where
import Language.Haskell.TH                              as H
import Data.Repa.Flow.Auto.Format                       as F
import Data.Repa.Query.Graph                            as G
import qualified Data.Repa.Store.Format                 as Q
import qualified Data.Repa.Convert.Formats              as C

import qualified Data.Repa.Query.Runtime.Primitive      as P
import qualified Data.Repa.Scalar.Product               as P


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression from a query scalar expression.
expOfExp   :: G.Exp () String String -> H.ExpQ 
expOfExp xx
 = case xx of
        G.XVal _ (G.VLit _ lit)
         -> expOfLit lit

        G.XVal _ (G.VLam _ sBind xBody)
         -> H.lamE [H.varP (H.mkName sBind)] (expOfExp xBody)

        G.XVar _ str
         -> H.varE (H.mkName str)

        G.XApp _ x1 x2
         -> H.appsE [expOfExp x1, expOfExp x2]

        G.XOp  _ sop xsArgs
         -> let Just hsop       = expOfScalarOp sop
            in  H.appsE (hsop : map expOfExp xsArgs)


-- | Yield a Haskell expression from a query scalar op.
expOfScalarOp :: ScalarOp -> Maybe H.ExpQ
expOfScalarOp sop
 = case sop of
        SopNeg          -> Just [| P.negate |]
        SopAbs          -> Just [| P.abs    |]
        SopSignum       -> Just [| P.signum |]
        SopAdd          -> Just [| P.add    |]
        SopSub          -> Just [| P.sub    |]
        SopMul          -> Just [| P.mul    |]
        SopDiv          -> Just [| P.div    |]
        SopEq           -> Just [| P.eq     |]
        SopNeq          -> Just [| P.neq    |]
        SopLe           -> Just [| P.le     |]
        SopGt           -> Just [| P.gt     |]
        SopGe           -> Just [| P.ge     |]
        SopLt           -> Just [| P.lt     |]

        SopRow 0
         -> Just [| P.Unit |]
        SopRow 1
         -> Just [| (\  a
                     -> a :*: P.Unit) |]
        SopRow 2
         -> Just [| (\  a b
                     -> a :*: b :*: P.Unit) |]
        SopRow 3
         -> Just [| (\  a b c
                     -> a :*: b :*: c :*: P.Unit) |]
        SopRow 4
         -> Just [| (\  a b c d
                     -> a :*: b :*: c :*: d :*: P.Unit) |]
        SopRow 5
         -> Just [| (\  a b c d e
                     -> a :*: b :*: c :*: d :*: e :*: P.Unit) |]
        SopRow 6
         -> Just [| (\  a b c d e f 
                     -> a :*: b :*: c :*: d :*: e :*: f :*: P.Unit) |]
        SopRow 7
         -> Just [| (\  a b c d e f g
                     -> a :*: b :*: c :*: d :*: e :*: f :*: g :*: P.Unit) |]
        SopRow 8
         -> Just [| (\  a b c d e f g h
                     -> a :*: b :*: c :*: d :*: e :*: f :*: g :*: h :*: P.Unit) |]
        SopRow 9
         -> Just [| (\  a b c d e f g h i 
                     -> a :*: b :*: c :*: d :*: e :*: f :*: g :*: h :*: i :*: P.Unit) |]
        SopRow _        
         -> Nothing


        SopGet 2 1      -> Just [| (\(x :*: _ :*: P.Unit)        -> x) |]
        SopGet 2 2      -> Just [| (\(_ :*: x :*: P.Unit)        -> x) |]

        SopGet 3 1      -> Just [| (\(x :*: _ :*: _ :*: P.Unit)  -> x) |]
        SopGet 3 2      -> Just [| (\(_ :*: x :*: _ :*: P.Unit)  -> x) |]
        SopGet 3 3      -> Just [| (\(_ :*: _ :*: x :*: P.Unit)  -> x) |]

        SopGet 4 1      -> Just [| (\(x :*: _ :*: _ :*: _ :*: P.Unit)  -> x) |]
        SopGet 4 2      -> Just [| (\(_ :*: x :*: _ :*: _ :*: P.Unit)  -> x) |]
        SopGet 4 3      -> Just [| (\(_ :*: _ :*: x :*: _ :*: P.Unit)  -> x) |]
        SopGet 4 4      -> Just [| (\(_ :*: _ :*: _ :*: x :*: P.Unit)  -> x) |]

        SopGet 5 1      -> Just [| (\(x :*: _ :*: _ :*: _ :*: _ :*: P.Unit) -> x) |]
        SopGet 5 2      -> Just [| (\(_ :*: x :*: _ :*: _ :*: _ :*: P.Unit) -> x) |]
        SopGet 5 3      -> Just [| (\(_ :*: _ :*: x :*: _ :*: _ :*: P.Unit) -> x) |]
        SopGet 5 4      -> Just [| (\(_ :*: _ :*: _ :*: x :*: _ :*: P.Unit) -> x) |]
        SopGet 5 5      -> Just [| (\(_ :*: _ :*: _ :*: _ :*: x :*: P.Unit) -> x) |]

        SopGet _ _      -> Nothing

        SopStringOfDate -> Just [| P.stringOfDate |]
        SopYearOfDate   -> Just [| P.yearOfDate   |]
        SopMonthOfDate  -> Just [| P.monthOfDate  |]
        SopDayOfDate    -> Just [| P.dayOfDate    |]


-- | Yield a Haskell literal from a query literal.
expOfLit :: G.Lit -> H.ExpQ
expOfLit lit 
 = case lit of
        G.LBool   True  -> [| True  |]
        G.LBool   False -> [| False |]

        G.LWord   w     -> return $ H.LitE $ H.IntegerL  w
        G.LInt    i     -> return $ H.LitE $ H.IntegerL  i
        G.LFloat  f     -> return $ H.LitE $ H.RationalL (toRational f)
        G.LDouble d     -> return $ H.LitE $ H.RationalL (toRational d)

        G.LString s     -> return $ H.LitE $ H.StringL   s


---------------------------------------------------------------------------------------------------
-- | Create a Haskell expression for a field mask.
expOfMask :: Int -> [Int] -> ExpQ
expOfMask len wanted
 = go [0 .. len - 1]
 where
        go []                   = [| P.Unit |]
        go (i : is)
         | elem i wanted        = [| P.Keep :*: $(go is) |]
         | otherwise            = [| P.Drop :*: $(go is) |]


---------------------------------------------------------------------------------------------------
-- | Yield a Haskell expression for a row format.
expOfDelimFields :: Q.Delim -> [Q.FieldBox] -> Maybe H.ExpQ
expOfDelimFields delim fields
 = case (delim, fields) of
        (Q.Fixed, [f])
         |  Just f'     <- expOfFieldFormat f
         -> Just f'

        (Q.Fixed, (f:fs))
         |  Just ff'    <- expOfFieldFormats f fs
         -> Just [| C.App $ff' |]

        (Q.Lines, [f])
         |  Just f'     <- expOfFieldFormat f
         -> Just f'

        (Q.LinesSep _c, [f])
         |  Just f'     <- expOfFieldFormat f
         -> Just f'

{-
        (Q.LinesSep c,  (f:fs))
         |  Just ff'    <- expOfFieldFormats f fs
         -> Just [| P.Sep $(H.litE (H.charL c)) $ff' |]
-}
        _ -> Nothing


-- | Yield a Haskell expression for a delimitor.
expOfDelim :: Q.Delim -> H.ExpQ
expOfDelim d
 = case d of
        Q.Fixed         -> [| P.Fixed |]
        Q.Lines         -> [| P.Lines |]
        Q.LinesSep c    -> [| P.LinesSep $(H.litE (H.charL c)) |]


-- | Yield a Haskell expression for some fields.
expOfFieldsFormat :: [Q.FieldBox] -> Maybe H.ExpQ
expOfFieldsFormat []
        = Nothing

expOfFieldsFormat (f : fs)
        = expOfFieldFormats f fs


-- | Yield a Haskell expression for some fields.
expOfFieldFormats :: Q.FieldBox -> [Q.FieldBox] -> Maybe H.ExpQ 
expOfFieldFormats f1 []
        | Just f1'      <- expOfFieldFormat f1
        = Just [| $f1' |]

expOfFieldFormats f1 (f2 : fs) 
        | Just f1'      <- expOfFieldFormat  f1
        , Just ff'      <- expOfFieldFormats f2 fs
        = Just [| $f1' P.:*: $ff'   |]

expOfFieldFormats _ _
        = Nothing


-- | Yield a Haskell expression for a field format.
expOfFieldFormat   :: Q.FieldBox -> Maybe H.ExpQ
expOfFieldFormat (Q.FieldBox field)
 = case field of
        Q.Nil           -> Just [| P.Unit      |]

        Q.Word8be       -> Just [| P.Word8be   |]
        Q.Int8be        -> Just [| P.Int8be    |]

        Q.Word16be      -> Just [| P.Word16be  |]
        Q.Int16be       -> Just [| P.Int16be   |]

        Q.Word32be      -> Just [| P.Word32be  |]
        Q.Int32be       -> Just [| P.Int32be   |] 

        Q.Word64be      -> Just [| P.Word64be  |]
        Q.Int64be       -> Just [| P.Int64be   |]

        Q.Float32be     -> Just [| P.Float32be |]
        Q.Float64be     -> Just [| P.Float64be |]

        Q.YYYYsMMsDD c  -> Just [| P.YYYYsMMsDD $(H.litE (H.charL c)) |]
        Q.DDsMMsYYYY c  -> Just [| P.DDsMMsYYYY $(H.litE (H.charL c)) |]

        Q.IntAsc        -> Just [| P.IntAsc    |]
        Q.DoubleAsc     -> Just [| P.DoubleAsc |]

{-
        Q.FixCharList len -> Just [| P.FixCharList $(H.litE (H.integerL (fromIntegral len))) |]
        Q.VarCharList     -> Just [| P.VarCharList    |]
-}
        _                 -> Nothing


---------------------------------------------------------------------------------------------------
expOfNat :: Int -> Maybe H.ExpQ
expOfNat n
 = case n of
        0       -> Just [| P.nat0 |]
        1       -> Just [| P.nat1 |]
        2       -> Just [| P.nat2 |]
        3       -> Just [| P.nat3 |]
        4       -> Just [| P.nat4 |]
        5       -> Just [| P.nat5 |]
        6       -> Just [| P.nat6 |]
        7       -> Just [| P.nat7 |]
        8       -> Just [| P.nat8 |]
        9       -> Just [| P.nat9 |]
        _       -> Nothing


