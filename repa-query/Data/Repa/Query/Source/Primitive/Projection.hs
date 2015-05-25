
module Data.Repa.Query.Source.Primitive.Projection
        ( row0, row1, row2, row3, row4, row5, row6, row7, row8, row9
        , get1_1
        , get2_1, get2_2
        , get3_1, get3_2, get3_3
        , get4_1, get4_2, get4_3, get4_4
        , get5_1, get5_2, get5_3, get5_4, get5_5)
where
import Data.Repa.Query.Source.Builder           as S
import Data.Repa.Query.Graph.Compounds          as G
import Data.Repa.Query.Graph                    as G
import Data.Repa.Scalar.Product


----------------------------------------------------------------------------------------------- Row
row0    :: Value ()
row0    = makeScalarOp0 $ G.SopRow 0


row1    :: Value a 
        -> Value (a :*: ())
row1    = makeScalarOp1 $ G.SopRow 1


row2    :: Value a -> Value b 
        -> Value (a :*: b :*: ())
row2    = makeScalarOp2 $ G.SopRow 2


row3    :: Value a -> Value b -> Value c 
        -> Value (a :*: b :*: c :*: ())
row3    = makeScalarOp3 $ G.SopRow 3


row4    :: Value a -> Value b -> Value c -> Value d 
        -> Value (a :*: b :*: c :*: d :*: ())
row4    = makeScalarOp4 $ G.SopRow 4


row5    :: Value a -> Value b -> Value c -> Value d -> Value e
        -> Value (a :*: b :*: c :*: d :*: e :*: ())
row5    = makeScalarOp5 $ G.SopRow 5


row6    :: Value a -> Value b -> Value c -> Value d -> Value e -> Value f
        -> Value (a :*: b :*: c :*: d :*: e :*: f :*: ())
row6    = makeScalarOp6 $ G.SopRow 6


row7    :: Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value g
        -> Value (a :*: b :*: c :*: d :*: e :*: f :*: g :*: ())
row7    = makeScalarOp7 $ G.SopRow 7


row8    :: Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value g -> Value h
        -> Value (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h :*: ())
row8    = makeScalarOp8 $ G.SopRow 8


row9    :: Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value g -> Value h 
        -> Value i
        -> Value (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h :*: i :*: ())
row9    = makeScalarOp9 $ G.SopRow 9


----------------------------------------------------------------------------------------------- Get
-- 1
get1_1  :: Value (a :*: ()) -> Value a
get1_1  = makeScalarOp1 $ G.SopGet 1 1 


-- 2
get2_1  :: Value (a :*: b :*: ()) -> Value a
get2_1  = makeScalarOp1 $ G.SopGet 2 1

get2_2  :: Value (a :*: b :*: ()) -> Value b
get2_2  = makeScalarOp1 $ G.SopGet 2 1


-- 3
get3_1  :: Value (a :*: b :*: c :*: ()) -> Value a
get3_1  = makeScalarOp1 $ G.SopGet 3 1

get3_2  :: Value (a :*: b :*: c :*: ()) -> Value b
get3_2  = makeScalarOp1 $ G.SopGet 3 2

get3_3  :: Value (a :*: b :*: c :*: ()) -> Value c
get3_3  = makeScalarOp1 $ G.SopGet 3 3


-- 4
get4_1  :: Value (a :*: b :*: c :*: d :*: ()) -> Value a
get4_1  = makeScalarOp1 $ G.SopGet 4 1

get4_2  :: Value (a :*: b :*: c :*: d :*: ()) -> Value b
get4_2  = makeScalarOp1 $ G.SopGet 4 2

get4_3  :: Value (a :*: b :*: c :*: d :*: ()) -> Value c
get4_3  = makeScalarOp1 $ G.SopGet 4 3

get4_4  :: Value (a :*: b :*: c :*: d :*: ()) -> Value d
get4_4  = makeScalarOp1 $ G.SopGet 4 4


-- 5
get5_1  :: Value (a :*: b :*: c :*: d :*: e :*: ()) -> Value a
get5_1  = makeScalarOp1 $ G.SopGet 5 1

get5_2  :: Value (a :*: b :*: c :*: d :*: e :*: ()) -> Value b
get5_2  = makeScalarOp1 $ G.SopGet 5 2

get5_3  :: Value (a :*: b :*: c :*: d :*: e :*: ()) -> Value c
get5_3  = makeScalarOp1 $ G.SopGet 5 3

get5_4  :: Value (a :*: b :*: c :*: d :*: e :*: ()) -> Value d
get5_4  = makeScalarOp1 $ G.SopGet 5 4

get5_5  :: Value (a :*: b :*: c :*: d :*: e :*: ()) -> Value e
get5_5  = makeScalarOp1 $ G.SopGet 5 5

