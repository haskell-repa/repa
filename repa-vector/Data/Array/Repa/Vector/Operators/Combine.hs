
module Data.Array.Repa.Vector.Operators.Combine
        ( vcombineByTag2
        , vcombineByFlag2)
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Base
import Data.Array.Repa
import qualified Data.Array.Repa.Stream as S
import qualified Data.Array.Repa.Chain  as C
import qualified Data.Vector.Unboxed    as U
import Prelude                          as P


-- | TODO: make this parallel.
vcombineByTag2 
        :: U.Unbox a
        => Vector U Int
        -> Vector U a -> Vector U a
        -> Vector U a

vcombineByTag2 (AUnboxed sh tags) (AUnboxed _ vec1) (AUnboxed _ vec2)
        = AUnboxed sh
        $ S.unstreamUnboxed
        $ S.combine2ByTag 
                (S.streamUnboxed tags)
                (S.streamUnboxed vec1)
                (S.streamUnboxed vec2)


vcombineByFlag2 
        :: (U.Unbox a, Show a)
        => Vector U Bool
        -> Vector U a -> Vector U a
        -> Vector U a

vcombineByFlag2 (AUnboxed sh tags) (AUnboxed _ vec1) (AUnboxed _ vec2)
 = AUnboxed sh
        $ S.unstreamUnboxed
        $ S.combine2ByTag 
                (S.streamUnboxed $ U.map fromFlag tags)
                (S.streamUnboxed vec1)
                (S.streamUnboxed vec2)

        where   fromFlag False = 0
                fromFlag True  = 1
                

