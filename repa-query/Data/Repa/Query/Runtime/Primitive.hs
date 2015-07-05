{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Primitive functions used by produced Repa code when compiling the queries.
--   All names used by the emitted query code are defined here so we can keep
--   track of what is being used.
--
module Data.Repa.Query.Runtime.Primitive
        ( -- * From Prelude
          pattern Unit
        , (>>=), (=<<), return
        , error

        , negate, abs, signum
        , add, sub, mul, div
        , eq,  neq
        , gt,  ge,  lt,  le

          -- * From Data.Repa.Bits.Date32
        , stringOfDate
        , yearOfDate
        , monthOfDate
        , dayOfDate

          -- * From Data.Repa.Flow.Auto
        , Sources, Sinks
        , map_i
        , zipWith_i, zipWith3_i, zipWith4_i, zipWith5_i, zipWith6_i, zipWith7_i
        , select_i
        , discard_i
        , mask_i
        , folds_i
        , groupsBy_i

          -- * From Data.Repa.Flow.Auto.IO
        , fromFiles
        , sourceLinesFormat
        , sourceTableFormat
        , sourceFamilyKey
        , sourceFamilyColumn

          -- * From Data.Repa.Flow.Auto.Format
        , packFormat_i
        , packFormatLn_i
        , packAsciiLn_i
        , keyPackAsciiLn_i

          -- * From Data.Repa.Store.Format
        , pattern Fixed
        , pattern Lines
        , pattern LinesSep

          -- * From Data.Repa.Convert.Format
        , pattern Word8be,      pattern Int8be
        , pattern Word16be,     pattern Int16be
        , pattern Word32be,     pattern Int32be
        , pattern Word64be,     pattern Int64be
        , pattern Float32be
        , pattern Float64be
        , pattern YYYYsMMsDD
        , pattern DDsMMsYYYY
        , pattern IntAsc
        , pattern DoubleAsc
        , pattern FixChars
        , pattern VarChars

        -- * From Data.Repa.Convert.Default.Ascii
        , formatAscii

        -- * From Data.Repa.Singleton.Nat
        , nat0, nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9

        -- * From System.FilePath
        , (</>)
        , (<.>))

where
import qualified Prelude                                as P
import qualified Data.Repa.Flow.Auto                    as F
import qualified Data.Repa.Flow.Auto.IO                 as F
import qualified Data.Repa.Flow.Auto.Format             as F
import qualified Data.Repa.Store.Flow                   as S
import qualified Data.Repa.Convert.Formats              as C    
import qualified Data.Repa.Convert                      as C
import qualified Data.Repa.Scalar.Date32                as B
import qualified Data.Repa.Scalar.Singleton.Nat         as R
import qualified System.FilePath                        as FilePath
import Data.Repa.Scalar.Product


-- Prelude ----------------------------------------------------------------------------------------
pattern Unit            = ()

(>>=)                   = (\x y -> x P.>>= y)
(=<<)                   = (\x y -> x P.=<< y)
return                  = P.return
error                   = P.error

negate                  = (\x   -> P.negate x)
abs                     = (\x   -> P.abs    x)
signum                  = (\x   -> P.signum x)
add                     = (\x y -> x P.+  y)
sub                     = (\x y -> x P.-  y)
mul                     = (\x y -> x P.*  y)
div                     = (\x y -> x P./  y)
eq                      = (\x y -> x P.== y)
neq                     = (\x y -> x P./= y)
gt                      = (\x y -> x P.>  y)
ge                      = (\x y -> x P.>= y)
lt                      = (\x y -> x P.<  y)
le                      = (\x y -> x P.<= y)


-- Data.Repa.Bits.Date32 --------------------------------------------------------------------------
stringOfDate            :: B.Date32 -> P.String
stringOfDate            = (\x -> let P.Just str = C.packToString (F.YYYYsMMsDD '-') x in str)

yearOfDate              :: B.Date32 -> P.Int
yearOfDate              = (\x -> P.fromIntegral (B.year  x))

monthOfDate             :: B.Date32 -> P.Int
monthOfDate             = (\x -> P.fromIntegral (B.month x))

dayOfDate               :: B.Date32 -> P.Int
dayOfDate               = (\x -> P.fromIntegral (B.day   x))


-- Data.Repa.Flow.Auto ----------------------------------------------------------------------------
-- Data.Repa.Flow.Auto
-- TODO: conversions need to be redone chunkwise,
--       instead of row-at-a-time.

type Sources a          = F.Sources a
type Sinks   a          = F.Sinks   a

map_i                   = F.map_i
zipWith_i               = F.zipWith_i
zipWith3_i              = F.zipWith3_i
zipWith4_i              = F.zipWith4_i
zipWith5_i              = F.zipWith5_i
zipWith6_i              = F.zipWith6_i
zipWith7_i              = F.zipWith7_i

select_i                = F.select_i
discard_i               = F.discard_i
mask_i                  = F.mask_i


-- Segmented fold.
-- TODO: do this chunkwise.
folds_i
        :: (F.FoldsDict n a b u1 u2 u3 u4, F.Flow n, F.Flow b)
        => (a -> b -> b)
        -> b
        -> F.Sources       (n :*: P.Int :*: ())
        -> F.Sources a 
        -> P.IO (F.Sources (n :*: b     :*: ()))

folds_i f z sNameLens sVals
 = do   sNameLens'      <- F.map_i   (\(name :*: len :*: ()) -> (name, len)) sNameLens
        sResult         <- F.folds_i f z sNameLens' sVals
        sResult'        <- F.map_i   (\(name, agg)           -> name :*: agg :*: ()) sResult
        return sResult'

-- Group by.
groupsBy_i 
        :: (F.GroupsDict a u1 u2, F.Flow a)
        => (a -> a -> P.Bool)
        -> F.Sources a
        -> P.IO (F.Sources (a :*: P.Int :*: ()))

groupsBy_i f ss
        =   F.map_i      (\(g, n) -> g :*: n :*: ())
        =<< F.groupsBy_i f ss



-- Data.Repa.Flow.Auto.IO -------------------------------------------------------------------------
fromFiles               = F.fromFiles
sourceLinesFormat       = F.sourceFormatLn
sourceTableFormat       = S.sourceTableFormat
sourceFamilyKey         = S.sourceFamilyKey
sourceFamilyColumn      = S.sourceFamilyColumn


-- Data.Repa.Flow.Auto.Format ---------------------------------------------------------------------
packFormat_i            = F.packFormat_i
packFormatLn_i          = F.packFormatLn_i
packAsciiLn_i           = F.packAsciiLn_i
keyPackAsciiLn_i        = F.keyPackAsciiLn_i


-- Data.Repa.Store.Format -------------------------------------------------------------------------
pattern Fixed           = S.Fixed
pattern Lines           = S.Lines
pattern LinesSep c      = S.LinesSep c


-- Data.Repa.Convert.Format -----------------------------------------------------------------------
pattern Word8be         = C.Word8be
pattern  Int8be         =  C.Int8be

pattern Word16be        = C.Word16be
pattern  Int16be        =  C.Int16be

pattern Word32be        = C.Word32be
pattern  Int32be        =  C.Int32be

pattern Word64be        = C.Word64be
pattern  Int64be        =  C.Int64be

pattern Float32be       = C.Float32be
pattern Float64be       = C.Float64be

pattern YYYYsMMsDD c    = C.YYYYsMMsDD c
pattern DDsMMsYYYY c    = C.DDsMMsYYYY c

pattern IntAsc          = C.IntAsc
pattern DoubleAsc       = C.DoubleAsc

pattern FixChars len    = C.FixChars len
pattern VarChars        = C.VarChars


-- Data.Repa.Convert.Default.Ascii ----------------------------------------------------------------
formatAscii             = C.formatAscii


-- Data.Repa.Singleton.Nat ------------------------------------------------------------------------
nat0                    = R.nat0
nat1                    = R.nat1
nat2                    = R.nat2
nat3                    = R.nat3
nat4                    = R.nat4
nat5                    = R.nat5
nat6                    = R.nat6
nat7                    = R.nat7
nat8                    = R.nat8
nat9                    = R.nat9


-- System.FilePath --------------------------------------------------------------------------------
(</>)                   = (\x y -> x FilePath.</> y)
(<.>)                   = (\x y -> x FilePath.<.> y)

