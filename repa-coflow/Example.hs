
module Example where
import Eater as E
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Unbox, Vector)

example1 :: Vector Int -> IO (Int, (Int, ()))
example1 xs
        =  E.sum  $ \eat1 
        -> E.prod $ \eat2
        -> E.ufeed1 xs (E.dup eat1 eat2)


example2 :: Vector Int -> IO (Int, (Int, ()))
example2 xs
        =  E.sum  $ \eat1 
        -> E.prod $ \eat2
        -> E.ufeed1 xs (E.dup (E.filter even eat1) eat2)


example3 :: Vector Int -> IO (Int, ())
example3 xs
        =  E.sum  $ \eat1
        -> E.ufeed1 xs (E.map (+ 1) eat1)
