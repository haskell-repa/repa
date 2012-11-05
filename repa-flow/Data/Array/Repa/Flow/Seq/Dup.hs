
module Data.Array.Repa.Flow.Dup
        (dup2)
where
import Data.Array.Repa.Flow.Base


dup2 :: Flow a -> IO (Flow a, Flow a)
dup2 (Flow getSize get1 get8)
 = do
        -- Start buffer at 1k elements.
        -- TODO: use a smaller start length if flow length is less.
        let startLen = (1024 :: Int)

        refLen   <- UM.unsafeNew 1
        UM.unsafeWrite refLen   0 startLen

        -- Front of buffer.
        refFront <- UM.unsafeNew 1
        UM.unsafeWrite refFront 0 (0 :: Int)

        -- Back of buffer.
        refBack  <- UM.unsafeNew 1
        UM.unsafeWrite refBack  0 (0 :: Int)

        let
         get1 push1
          = do  


        return $ Flow getSize get1 get8

