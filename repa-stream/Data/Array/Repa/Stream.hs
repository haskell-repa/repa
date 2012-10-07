
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

          -- * Constructors
        , stream,       streamD
        , stream'
        , streamOfChain

        -- * Packing
        , pack)
where
import Data.Array.Repa.Stream.Base
import Data.Array.Repa.Stream.Pack
import Data.Array.Repa.Distro
