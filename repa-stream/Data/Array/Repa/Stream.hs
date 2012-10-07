
module Data.Array.Repa.Stream
        ( -- * Linear Stream
          Stream        (..)
        , DistStream    (..)
        , Distro        (..)
        , Step          (..)
        , Size          (..)

          -- * Reduction
        , fold,         foldD
        , foldM,        foldMD

          -- * Evaluation
        , evalM,        evalMD

          -- * Constructors
        , stream,       streamD
        , stream'
        , streamOfChain

        -- * Packing
        , pack)
where
import Data.Array.Repa.Stream.Base
import Data.Array.Repa.Stream.Eval
import Data.Array.Repa.Stream.Pack
import Data.Array.Repa.Distro
--import qualified Data.Vector.Unboxed            as U
--import qualified Data.Vector.Unboxed.Mutable    as UM

