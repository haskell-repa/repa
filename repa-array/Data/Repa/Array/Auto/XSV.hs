
module Data.Repa.Array.Auto.XSV
        ( getArrayFromXSV,      hGetArrayFromXSV
        , putArrayAsXSV,        hPutArrayAsXSV)
where
import Data.Repa.Array.Auto.IO
import Data.Repa.Array.Auto.Base
import Data.Repa.Array.Generic.Convert
import Data.Char
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Nested        as A
import qualified Data.Repa.Array.Meta                   as A
import qualified Data.Repa.Array.Generic                as A
import qualified System.IO                              as S
import qualified System.FileLock                        as S


---------------------------------------------------------------------------------------------------
-- | Read a XSV file as a nested array.
--   We get an array of rows:fields:characters.
getArrayFromXSV
        :: Char                 -- ^ Field separator character, eg '|', ',' or '\t'.
        -> FilePath             -- ^ Source file handle.
        -> IO (Array (Array (Array Char)))

getArrayFromXSV !cSep !path
 = S.withFileLock path S.Shared
 $ \_lock -> do
        h       <- S.openFile path S.ReadMode
        arr     <- hGetArrayFromXSV cSep h
        S.hClose h
        return arr


-- | Read an XSV file as a nested array.
--   We get an array of rows:fields:characters.
hGetArrayFromXSV 
        :: Char                 -- ^ Field separator character, eg '|', ',' or '\t'.
        -> S.Handle             -- ^ Source file handle.
        -> IO (Array (Array (Array Char)))

hGetArrayFromXSV !cSep !hIn
 = do   
        -- Find out how much data there is remaining in the file.
        start     <- S.hTell hIn
        S.hSeek hIn S.SeekFromEnd 0
        end       <- S.hTell hIn
        let !len  =  end - start
        S.hSeek hIn S.AbsoluteSeek start

        -- Read array as Word8s.
        !arr8   <- hGetArray hIn (fromIntegral len)

        -- Rows are separated by new lines,
        -- fields are separated by the given separator character.
        let !nl = fromIntegral $ ord '\n'
        let !nc = fromIntegral $ ord cSep

        -- Split XSV file into rows and fields.
        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        let !arrChar 
                = A.mapElems 
                        (A.mapElems (A.computeS A.F . A.map (chr . fromIntegral))) 
                        (A.diceSep nc nl arr8)

        return $ convert arrChar


--------------------------------------------------------------------------------------------------
-- | Write a nested array as an XSV file.
--
--   The array contains rows:fields:characters.
putArrayAsXSV
        :: Char                         -- ^ Separator character, eg '|', ',' or '\t'
        -> FilePath                     -- ^ Source file handle.
        -> Array (Array (Array Char))   -- ^ Array of row, field, character.
        -> IO ()

putArrayAsXSV !cSep !path !arrChar
 = S.withFileLock path S.Exclusive
 $ \_lock -> do
        h       <- S.openFile path S.WriteMode
        hPutArrayAsXSV cSep h arrChar
        S.hClose h


-- | Write a nested array as an XSV file.
--
--   The array contains rows:fields:characters.
hPutArrayAsXSV
        :: Char                         -- ^ Separator character, eg '|', ',' or '\t'
        -> S.Handle                     -- ^ Source file handle.
        -> Array (Array (Array Char))   -- ^ Array of row, field, character.
        -> IO ()

hPutArrayAsXSV !cSep !hOut !arrChar
 = do
        -- Concat result back into Word8s
        let !arrC       = A.fromList A.U [cSep]
        let !arrNL      = A.fromList A.U ['\n']

        let !arrOut     
                = A.mapS A.A (fromIntegral . ord) 
                $ A.concat A.U 
                $ A.mapS A.B (\  arrFields
                              -> A.concat A.U $ A.fromList A.B
                                        [ A.intercalate A.U arrC arrFields, arrNL])
                $ arrChar

        hPutArray hOut arrOut
{-# INLINE hPutArrayAsXSV #-}

