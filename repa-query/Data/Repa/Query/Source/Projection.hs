
module Data.Repa.Query.Source.Projection
        ( get2_1, get2_2
        , get3_1, get3_2, get3_3
        , get4_1, get4_2, get4_3, get4_4
        , get5_1, get5_2, get5_3, get5_4, get5_5)
where
import Data.Repa.Query.Source.Builder           as S
import Data.Repa.Query.Source.Compounds         as S
import Data.Repa.Query.Graph                    as G
import Data.Repa.Product


-- 2
get2_1  :: Value (a :*: b)                        -> Value a
get2_1  = makeScalarOp1 $ G.SopProj 2 1

get2_2  :: Value (a :*: b)                        -> Value b
get2_2  = makeScalarOp1 $ G.SopProj 2 1


-- 3
get3_1  :: Value (a :*: b :*: c)                  -> Value a
get3_1  = makeScalarOp1 $ G.SopProj 3 1

get3_2  :: Value (a :*: b :*: c)                  -> Value b
get3_2  = makeScalarOp1 $ G.SopProj 3 2

get3_3  :: Value (a :*: b :*: c)                  -> Value c
get3_3  = makeScalarOp1 $ G.SopProj 3 3


-- 4
get4_1  :: Value (a :*: b :*: c :*: d)            -> Value a
get4_1  = makeScalarOp1 $ G.SopProj 4 1

get4_2  :: Value (a :*: b :*: c :*: d)            -> Value b
get4_2  = makeScalarOp1 $ G.SopProj 4 2

get4_3  :: Value (a :*: b :*: c :*: d)            -> Value c
get4_3  = makeScalarOp1 $ G.SopProj 4 3

get4_4  :: Value (a :*: b :*: c :*: d)            -> Value d
get4_4  = makeScalarOp1 $ G.SopProj 4 4


-- 5
get5_1  :: Value (a :*: b :*: c :*: d :*: e)      -> Value a
get5_1  = makeScalarOp1 $ G.SopProj 5 1

get5_2  :: Value (a :*: b :*: c :*: d :*: e)      -> Value b
get5_2  = makeScalarOp1 $ G.SopProj 5 2

get5_3  :: Value (a :*: b :*: c :*: d :*: e)      -> Value c
get5_3  = makeScalarOp1 $ G.SopProj 5 3

get5_4  :: Value (a :*: b :*: c :*: d :*: e)      -> Value d
get5_4  = makeScalarOp1 $ G.SopProj 5 4

get5_5  :: Value (a :*: b :*: c :*: d :*: e)      -> Value e
get5_5  = makeScalarOp1 $ G.SopProj 5 5

