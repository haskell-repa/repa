{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Conversion of Repa queries to JSON format for interfacing with
--   client systems.
-- 
--   Conversion is done by providing instances for the `ToJSON` and `FromJSON`
--   classes form the Aeson package.
--
module Data.Repa.Query.Convert.JSON
        (encode, toJSON, fromJSON, Result(..))
where
import Control.Monad
import Data.Repa.Query.Graph
import Data.Repa.Query.Exp
import Data.Aeson                               as Aeson
import Data.Text                                (Text)
import qualified Data.Repa.Query.Format         as Format
import qualified Data.Text                      as T
import qualified Data.HashMap.Strict            as H



--------------------------------------------------------------------------------------------- Query
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (Query a nF bV nV)) where
 toJSON xx
  = case xx of
        Query flow format graph
         -> object [ "type"     .= text "query"
                   , "out"      .= toJSON flow
                   , "row"      .= toJSON format
                   , "graph"    .= toJSON graph ]


instance (FromJSON nF, FromJSON bV, FromJSON nV)
      => (FromJSON (Query () nF bV nV)) where
 parseJSON (Object hh)

        | Just (String "query") <- H.lookup "type"  hh
        , Just jOut             <- H.lookup "out"   hh
        , Just jRow             <- H.lookup "row"   hh
        , Just jGraph           <- H.lookup "graph" hh
        = do    out     <- parseJSON jOut
                row     <- parseJSON jRow
                graph   <- parseJSON jGraph
                return  $ Query out row graph

 parseJSON _ = mzero
 

--------------------------------------------------------------------------------------------- Graph
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (Graph a nF bV nV)) where
 toJSON xx
  = case xx of
        Graph ns
         -> object [ "type"     .= text "graph"
                   , "nodes"    .= toJSON ns ]

instance (FromJSON nF, FromJSON bV, FromJSON nV)
      => (FromJSON (Graph () nF bV nV)) where
 parseJSON (Object hh)

        | Just (String "graph") <- H.lookup "type"  hh
        , Just jNodes           <- H.lookup "nodes" hh
        = do    nodes  <- parseJSON jNodes
                return  $ Graph nodes 

 parseJSON _ = mzero


---------------------------------------------------------------------------------------------- Node
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (Node a nF bV nV)) where
 toJSON xx
  = case xx of
        NodeSource s
         -> object [ "type"     .= text "node"
                   , "node"     .= text "source"
                   , "source"   .= toJSON s ]

        NodeOp op
         -> object [ "type"     .= text "node"
                   , "node"     .= text "op"
                   , "op"       .= toJSON op ]


instance (FromJSON nF, FromJSON bV, FromJSON nV)
      => (FromJSON (Node () nF bV nV)) where
 parseJSON (Object hh)

        | Just (String "node")   <- H.lookup "type"    hh
        , Just (String "source") <- H.lookup "node"    hh
        , Just jSource           <- H.lookup "source"  hh
        = do    source  <- parseJSON jSource
                return  $ NodeSource source

        | Just (String "node")  <- H.lookup "type"     hh
        , Just (String "op")    <- H.lookup "node"     hh
        , Just jOp              <- H.lookup "op"       hh
        = do    op      <- parseJSON jOp
                return  $ NodeOp op

 parseJSON _ = mzero


-------------------------------------------------------------------------------------------- FlowOp
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (FlowOp a nF bV nV)) where
 toJSON xx
  = case xx of
        FopMapI fIn fOut fun
         -> object [ "type"     .= text "fop"
                   , "fop"      .= text "mapi"
                   , "in"       .= toJSON fIn
                   , "out"      .= toJSON fOut
                   , "fun"      .= toJSON fun ]

        FopFilterI fIn fOut fun
         -> object [ "type"     .= text "fop"
                   , "fop"      .= text "filteri"
                   , "in"       .= toJSON fIn
                   , "out"      .= toJSON fOut
                   , "fun"      .= toJSON fun ]

        FopFoldI fIn fOut fun z
         -> object [ "type"     .= text "fop"
                   , "fop"      .= text "foldi"
                   , "in"       .= toJSON fIn
                   , "out"      .= toJSON fOut
                   , "fun"      .= toJSON fun 
                   , "neutral"  .= toJSON z ]

        FopFoldsI fLens fElems fOut fun z
         -> object [ "type"     .= text "fop"
                   , "fop"      .= text "folds"
                   , "lens"     .= toJSON fLens
                   , "elems"    .= toJSON fElems
                   , "out"      .= toJSON fOut
                   , "fun"      .= toJSON fun 
                   , "neutral"  .= toJSON z ]

        FopGroupsI fIn fOut fun
         -> object [ "type"     .= text "fop"
                   , "fop"      .= text "groupsi"
                   , "in"       .= toJSON fIn
                   , "out"      .= toJSON fOut
                   , "fun"      .= toJSON fun ]


instance (FromJSON nF, FromJSON bV, FromJSON nV)
       => FromJSON (FlowOp () nF bV nV) where
 parseJSON (Object hh)

        -- mapi
        | Just (String "fop")  <- H.lookup "type"   hh
        , Just (String "mapi") <- H.lookup "fop"    hh
        , Just jIn             <- H.lookup "in"     hh
        , Just jOut            <- H.lookup "out"    hh
        , Just jFun            <- H.lookup "fun"    hh
        = do    fin     <- parseJSON jIn
                fout    <- parseJSON jOut
                fun     <- parseJSON jFun
                return  $  FopMapI fin fout fun

        -- filteri
        | Just (String "fop")   <- H.lookup "type"  hh
        , Just (String "filteri") <- H.lookup "fop" hh
        , Just jIn             <- H.lookup "in"     hh
        , Just jOut            <- H.lookup "out"    hh
        , Just jFun            <- H.lookup "fun"    hh
        = do    fin     <- parseJSON jIn
                fout    <- parseJSON jOut
                fun     <- parseJSON jFun
                return  $  FopFilterI fin fout fun

        -- foldi
        | Just (String "fop")   <- H.lookup "type"     hh
        , Just (String "foldi") <- H.lookup "fop"      hh
        , Just jIn              <- H.lookup "in"       hh
        , Just jOut             <- H.lookup "out"      hh
        , Just jFun             <- H.lookup "fun"      hh
        , Just jNeutral         <- H.lookup "neutral"  hh
        = do    fin     <- parseJSON jIn
                fout    <- parseJSON jOut
                fun     <- parseJSON jFun
                neutral <- parseJSON jNeutral
                return  $  FopFoldI fin fout fun neutral

        -- foldsi
        | Just (String "fop")    <- H.lookup "type"    hh
        , Just (String "foldsi") <- H.lookup "fop"     hh
        , Just jLens             <- H.lookup "lens"    hh
        , Just jElems            <- H.lookup "elems"   hh
        , Just jOut              <- H.lookup "out"     hh
        , Just jFun              <- H.lookup "fun"     hh
        , Just jNeutral          <- H.lookup "neutral" hh
        = do    flens   <- parseJSON jLens
                felems  <- parseJSON jElems
                fout    <- parseJSON jOut
                fun     <- parseJSON jFun
                neutral <- parseJSON jNeutral
                return  $  FopFoldsI flens felems fout fun neutral

        -- groupsi
        | Just (String "fop")     <- H.lookup "type"  hh
        , Just (String "groupsi") <- H.lookup "fop" hh
        , Just jIn              <- H.lookup "in"     hh
        , Just jOut             <- H.lookup "out"    hh
        , Just jFun             <- H.lookup "fun"    hh
        = do    fin     <- parseJSON jIn
                fout    <- parseJSON jOut
                fun     <- parseJSON jFun
                return  $  FopFilterI fin fout fun

 parseJSON _ = mzero


-------------------------------------------------------------------------------------------- Source
instance (ToJSON nF)
       => ToJSON (Source a nF) where
 toJSON xx
  = case xx of
        SourceTable _ name format fOut
         -> object [ "type"     .= text "source"
                   , "source"   .= text "table"
                   , "name"     .= T.pack name
                   , "format"   .= toJSON format
                   , "output"   .= toJSON fOut ]


instance  FromJSON nF
       => FromJSON (Source () nF) where
 parseJSON (Object hh)

        | Just (String "source") <- H.lookup "type"   hh
        , Just (String "table")  <- H.lookup "source" hh
        , Just (String  name)    <- H.lookup "name"   hh
        , Just jFormat           <- H.lookup "format" hh
        , Just jOut              <- H.lookup "output" hh
        = do  format  <- parseJSON jFormat
              out     <- parseJSON jOut
              return  $ SourceTable () (T.unpack name) format out

 parseJSON _ = mzero


----------------------------------------------------------------------------------------------- Exp
instance (ToJSON bV, ToJSON nV)
       => ToJSON (Exp a bV nV) where
 toJSON xx
  = case xx of
        -- literals
        XVal _ (VLit _ (LBool b))
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "bool"
                   , "value"    .= T.pack (show b) ]

        XVal _ (VLit _ (LInt i))
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "int"
                   , "value"    .= T.pack (show i) ]

        XVal _ (VLit _ (LFloat f))
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "float"
                   , "value"    .= T.pack (show f) ]

        XVal _ (VLit _ (LString s))
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "string"
                   , "value"    .= T.pack s ]

        -- lambdas
        XVal _ (VLam _ bV x)
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lam"
                   , "binder"   .= toJSON bV
                   , "body"     .= toJSON x ]

        -- variables
        XVar _ v
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "var"
                   , "var"      .= toJSON v ]

        -- applications
        XApp _ xFun xArg
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "app"
                   , "fun"      .= toJSON xFun
                   , "arg"      .= toJSON xArg ]

        -- operators
        XOp  _ sOp xsArgs
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "sop"
                   , "sop"      .= nameOfScalarOp sOp
                   , "args"     .= toJSON xsArgs ]


instance (FromJSON bV, FromJSON nV)
       => FromJSON (Exp () bV nV) where
 parseJSON (Object hh)

        -- literals
        | Just (String "exp")   <- H.lookup "type"  hh
        , Just (String "lit")   <- H.lookup "exp"   hh
        , Just (String  lit)    <- H.lookup "lit"   hh
        , Just (String  value)  <- H.lookup "value" hh
        = case T.unpack lit of
              "int"             -> return $ xInt    () $ read $ T.unpack value
              "float"           -> return $ xFloat  () $ read $ T.unpack value
              "string"          -> return $ xString () $ T.unpack value
              _                 -> mzero

        -- variables
        | Just (String "exp")   <- H.lookup "type"   hh
        , Just (String "var")   <- H.lookup "exp"    hh
        , Just jName            <- H.lookup "var"    hh
        = do  name      <- parseJSON jName
              return $ XVar () name

        -- lambdas
        | Just (String "exp")   <- H.lookup "type"   hh
        , Just (String "lam")   <- H.lookup "exp"    hh
        , Just jBinder          <- H.lookup "binder" hh
        , Just jBody            <- H.lookup "body"   hh
        = do  binder    <- parseJSON jBinder
              body      <- parseJSON jBody
              return $ XVal () (VLam () binder body)

        -- operators
        | Just (String "exp")   <- H.lookup "type"   hh
        , Just (String "sop")   <- H.lookup "exp"    hh
        , Just (String ssop)    <- H.lookup "sop"    hh
        , Just jArgs            <- H.lookup "args"   hh
        , Just sop              <- scalarOpOfName (T.unpack ssop)
        = do  args      <- parseJSON jArgs
              return $ XOp  () sop args

 parseJSON _ = mzero


------------------------------------------------------------------------------------------ ScalarOp
nameOfScalarOp :: ScalarOp -> String
nameOfScalarOp sop
 = case sop of
        SopNeg          -> "neg"
        SopAdd          -> "add"
        SopSub          -> "sub"
        SopMul          -> "mul"
        SopDiv          -> "div"
        SopEq           -> "eq"
        SopNeq          -> "neq"
        SopGt           -> "gt"
        SopGe           -> "ge"
        SopLt           -> "lt"
        SopLe           -> "le"


scalarOpOfName :: String -> Maybe ScalarOp
scalarOpOfName ss
 = case ss of
        "neg"           -> Just $ SopNeg
        "add"           -> Just $ SopAdd
        "sub"           -> Just $ SopSub
        "mul"           -> Just $ SopMul
        "div"           -> Just $ SopDiv
        "eq"            -> Just $ SopEq
        "neq"           -> Just $ SopNeq
        "gt"            -> Just $ SopGt
        "ge"            -> Just $ SopGe
        "lt"            -> Just $ SopLt
        "le"            -> Just $ SopLe
        _               -> Nothing


-------------------------------------------------------------------------------------- Format.Row
instance ToJSON Format.Row where
 toJSON rr
  = case rr of
        Format.RowPacked ff
         -> object [ "type"     .= text "row"
                   , "row"      .= text "packed"
                   , "fields"   .= toJSON ff  ]

        Format.RowLineSep c ff
         -> object [ "type"     .= text "row"
                   , "row"      .= text "line"
                   , "sep"      .= T.pack [c]
                   , "fields"   .= toJSON ff ]


instance FromJSON Format.Row where
 parseJSON (Object hh)
        | Just (String "row")    <- H.lookup "type"   hh
        , Just (String "packed") <- H.lookup "row"    hh
        , Just jFields           <- H.lookup "fields" hh
        = do    fields  <- parseJSON jFields
                return  $ Format.RowPacked fields

        | Just (String "row")    <- H.lookup "type"   hh
        , Just (String "line")   <- H.lookup "row"    hh
        , Just (String sep)      <- H.lookup "sep"    hh
        , Just jFields           <- H.lookup "fields" hh
        , [c]                    <- T.unpack sep
        = do    fields  <- parseJSON jFields
                return  $ Format.RowLineSep c fields

 parseJSON _ = mzero


-------------------------------------------------------------------------------------- Format.Field
instance ToJSON Format.Field where
 toJSON ff
        = object  [ "type"      .= text "field"
                  , "field"     .= T.pack (Format.showField ff) ]


instance FromJSON Format.Field where
 parseJSON (Object hh)
        | Just (String "field") <- H.lookup "type"  hh
        , Just (String ff)      <- H.lookup "field" hh
        , Just f                <- Format.readField (T.unpack ff)
        = return f

 parseJSON _ = mzero


---------------------------------------------------------------------------------------------------
text :: Text -> Text
text x = x 



