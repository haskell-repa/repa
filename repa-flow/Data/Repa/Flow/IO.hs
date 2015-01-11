
module Data.Repa.Flow.IO
        

where



-- | Read complete lines of data from a text file, using the given chunk length.
fileSourcesWords
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> IO (Sources F Char)
fileSourcesLines files nChunk fails
 =   map_i F (chr . fromIntegral) 
 =<< G.fileSourcesRecords files nChunk isNewLine fails
 where  isNewLine   :: Word8 -> Bool
        isNewLine x =  x == (fromIntegral $ ord '\n')
        {-# INLINE isNewLine #-}
{-# INLINE fileSourcesLines #-}
