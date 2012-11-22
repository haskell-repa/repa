
module Data.Array.Repa.Vector.Operators.Combine
        ( vcombine2
        , vcombineSegs2)
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Base
import Data.Array.Repa
import qualified Data.Array.Repa.Stream as S
import qualified Data.Vector.Unboxed    as U
import Prelude                          as P


-- | TODO: make this parallel.
vcombine2
        :: U.Unbox a
        => Vector U Bool
        -> Vector U a -> Vector U a
        -> Vector U a

vcombine2 (AUnboxed sh tags) (AUnboxed _ vec1) (AUnboxed _ vec2)
 = AUnboxed sh
        $ S.unstreamUnboxed
        $ S.combine2
                (S.streamUnboxed tags)
                (S.streamUnboxed vec1)
                (S.streamUnboxed vec2)
                

vcombineSegs2 
        :: U.Unbox a
        => Vector U Bool
        -> Vector U Int -> Vector U a
        -> Vector U Int -> Vector U a
        -> Vector U a

vcombineSegs2 
        (AUnboxed sh flags)
        (AUnboxed _  lens1) (AUnboxed _ elems1)
        (AUnboxed _  lens2) (AUnboxed _ elems2)
 = AUnboxed sh
        $ S.unstreamUnboxed
        $ S.combineSegs2
                (S.streamUnboxed flags)
                (S.streamUnboxed lens1) (S.streamUnboxed elems1)
                (S.streamUnboxed lens2) (S.streamUnboxed elems2)
