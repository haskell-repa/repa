
module Data.Repa.Query.Source.Primitive.Scalar
        ( -- * Arithmetic
          negate, abs, signum
        , (+),  (-),  (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=)

          -- * Dates
        , stringOfDate
        , yearOfDate
        , monthOfDate
        , dayOfDate)
where
import Data.Repa.Query.Source.Builder
import Data.Repa.Query.Graph                    as G
import Data.Repa.Query.Graph.Compounds          as G
import Data.Repa.Scalar.Date32                  (Date32)
import Prelude   
 hiding ( negate, abs, signum
        , (+),  (-), (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))


-- TODO:  We can't determine the real type for 'a' at query build time.
-- Define a placeholder type Scalar and use it for 'a' in these functions.
-- When we've looked up the meta-data we can type-check the graph itself
-- to ensure the query will work.
--
--  should have
--   (+) :: Value Scalar -> Value Scalar -> Value Scalar
-- 
--  This will stop us from trying to add non-scalar values,
--  and we can defined an instance of the Num type class for it.
--


---------------------------------------------------------------------------------------- Arithmetic
-- | Scalar negation.
negate :: Value a -> Value a
negate = makeScalarOp1 G.SopNeg


-- | Scalar absolute value.
abs :: Value a -> Value a
abs    = makeScalarOp1 G.SopAbs


-- | Scalar sign of number.
signum :: Value a -> Value a
signum = makeScalarOp1 G.SopSignum


-- | Scalar addition.
(+) :: Value a -> Value a -> Value a
(+) = makeScalarOp2 G.SopAdd 


-- | Scalar subtraction.
(-) :: Value a -> Value a -> Value a
(-) = makeScalarOp2 G.SopSub


-- | Scalar multiplication.
(*) :: Value a -> Value a -> Value a
(*) = makeScalarOp2 G.SopMul


-- | Scalar division.
(/) :: Value a -> Value a -> Value a
(/) = makeScalarOp2 G.SopMul


-- | Scalar equality.
(==) :: Value a -> Value a -> Value Bool
(==) = makeScalarOp2 G.SopEq


-- | Scalar negated equality.
(/=) :: Value a -> Value a -> Value Bool
(/=) = makeScalarOp2 G.SopNeq


-- | Scalar greater-than.
(>) :: Value a -> Value a -> Value Bool
(>) = makeScalarOp2 G.SopGt


-- | Scalar greater-than-equal.
(>=) :: Value a -> Value a -> Value Bool
(>=) = makeScalarOp2 G.SopGe


-- | Scalar less-than.
(<) :: Value a -> Value a -> Value Bool
(<)  = makeScalarOp2 G.SopLt


-- | Scalar less-than-equal.
(<=) :: Value a -> Value a -> Value Bool
(<=) = makeScalarOp2 G.SopLe


--------------------------------------------------------------------------------------------- Dates
stringOfDate :: Value Date32 -> Value String
stringOfDate = makeScalarOp1 G.SopStringOfDate 

yearOfDate   :: Value Date32 -> Value Int
yearOfDate   = makeScalarOp1 G.SopYearOfDate

monthOfDate  :: Value Date32 -> Value Int
monthOfDate  = makeScalarOp1 G.SopMonthOfDate

dayOfDate    :: Value Date32 -> Value Int
dayOfDate    = makeScalarOp1 G.SopDayOfDate




