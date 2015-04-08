
module Data.Repa.Query.JSON
where
import Data.Repa.Query.Graph
import Data.Aeson                       as Aeson
import qualified Data.Text              as T
import Data.Text                        (Text)


--------------------------------------------------------------------------------------------- Graph
instance (ToJSON nF, ToJSON bV, ToJSON nV)
      => (ToJSON (Graph a nF bV nV)) where

 toJSON xx
  = case xx of
        Graph ns
         -> object [ "type"     .= text "graph"
                   , "nodes"    .= toJSON ns ]


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


-------------------------------------------------------------------------------------------- Source
instance (ToJSON nF)
       => ToJSON (Source a nF) where
 toJSON xx
  = case xx of
        SourceTable _ n fOut
         -> object [ "type"     .= text "source"
                   , "source"   .= text "table"
                   , "name"     .= T.pack n
                   , "output"   .= toJSON fOut ]


----------------------------------------------------------------------------------------------- Exp
instance (ToJSON bV, ToJSON nV)
       => ToJSON (Exp a bV nV) where
 toJSON xx
  = case xx of
        XLit _ (LitInt i)
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "int"
                   , "value"    .= T.pack (show i) ]

        XLit _ (LitFloat f)
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "float"
                   , "value"    .= T.pack (show f) ]

        XLit _ (LitString s)
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lit"
                   , "lit"      .= text "string"
                   , "value"    .= T.pack s ]

        XVar _ v
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "var"
                   , "var"      .= toJSON v ]

        XLam _ bV xx
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "lam"
                   , "binder"   .= toJSON bV
                   , "body"     .= toJSON xx ]

        XOp  _ sOp xsArgs
         -> object [ "type"     .= text "exp"
                   , "exp"      .= text "sop"
                   , "sop"      .= nameOfScalarOp sOp
                   , "args"     .= toJSON xsArgs ]


------------------------------------------------------------------------------------------ ScalarOp
nameOfScalarOp :: ScalarOp -> String
nameOfScalarOp sop
 = case sop of
        SopNeg      -> "neg"
        SopAdd      -> "add"
        SopSub      -> "sub"
        SopMul      -> "mul"
        SopDiv      -> "div"
        SopEq       -> "eq"
        SopNeq      -> "neq"
        SopGt       -> "gt"
        SopGe       -> "ge"
        SopLt       -> "lt"
        SopLe       -> "le"


---------------------------------------------------------------------------------------------------
text :: Text -> Text
text x = x 
