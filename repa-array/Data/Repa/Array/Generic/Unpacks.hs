
module Data.Repa.Array.Generic.Unpacks 
        (unsafeUnpacksToBuffer)
where
import Data.Repa.Convert.Format
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Layout

import Data.IORef
import Data.Word
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Ptr                    as F


-- | Given a buffer containing an encoded table where the values in each
--   column all have the same time, decode all the values from a single
--   column and write them to a buffer.
--
--   For example, suppose we have a table as follows, where the rows are
--   separated by newline characters and the field separated by spaces.
--
-- @
-- RED 1.0 0.0 0.0
-- GREEN 0.0 1.0 0.0
-- BLUE 0.0 0.0 1.0
-- CYAN 0.0 1.0 1.0
-- @
--
--   To decode the second column use:
--
--   * Format: DoubleAsc, as they are encoded doubles.
--   * Field separator: ' ' as the fields are separated by spaces.
--   * Starting offsets: [3, 21, 38, 55], which are the indices of the starting
--     character of each field in the second column.
--   * Ending offsets: [16, 34, 51, 68], which are the indices of the newline
--     characters.
--   * Destination buffer: an new buffer with at least as many elements as there
--     are lines in the input data.
--
--   If the parse succeeds then the buffer containing the starting offets is
--   updated so each element is the index of the NEXT field in each column.
--   This allows the client to easilly decode the next column.
--
--   If there was a parse error then this function returns a pair of the row
--   index and offset in the buffer of the field which could not be parsed.
--
--   UNSAFE: Both the buffer containing ending offsets, and the destination
--   buffer must be at least as long as the buffer containing starting offsets
--   but this is not checked. If this is not true then the function will
--   will perform an out of bounds access.
--
--   INLINE: This function is set to INLINE so that it will be specialised
--   at the call site for the given format. For standard formats it's better
--   to use the pre-specialised versions for Auto arrays.
--
unsafeUnpacksToBuffer
        :: forall format lStart lEnd lVal
        .  ( Unpackable format
           , TargetI lStart Int
           , TargetI lEnd   Int
           , TargetI lVal   (Value format))
        => format                       -- ^ Format for each element.
        -> Word8                        -- ^ Field separator character.
        -> Array  F Word8               -- ^ Packed binary source data.
        -> Buffer lStart Int            -- ^ Starting offsets for fields.
        -> Buffer lEnd   Int            -- ^ Ending   offsets of rows.
        -> Buffer lVal (Value format)   -- ^ Destination buffer for parsed fields.
        -> IO (Maybe (Int, Int))

unsafeUnpacksToBuffer
        format cTerm src 
        ixsStart ixsEnd
        bufOut
 = do
        refError        <- newIORef Nothing
        loop refError 0
        readIORef refError

 where
        -- Length of the column, in rows.
        !lenColumn
         = extent $ bufferLayout ixsStart

        -- Get unpack the starting pointer form the source buffer.
        !(offSrc, _lenSrc, fptrSrc)
         = toForeignPtr src

        -- Read all the fields of the column in turn.
        loop refError ixField
         | ixField >= lenColumn    
         = return ()

         | otherwise
         = F.withForeignPtr fptrSrc
         $ \ptrSrc 
         -> do   
                -- Starting offset of field in source array.
                ixStart <- unsafeReadBuffer ixsStart ixField

                -- Ending offset of row in source array.
                ixEnd   <- unsafeReadBuffer ixsEnd   ixField

                -- Get a pointer to the field data in the source array.
                let ptrStart 
                        = F.plusPtr ptrSrc (offSrc + ixStart)

                -- Unpack a single field.
                r       <- unsafeRunUnpacker 
                                (unpack format) 
                                ptrStart
                                (ixEnd - ixStart + 1)
                                (== cTerm)

                case r of
                 -- The field didn't parse.
                 Nothing        
                  -> writeIORef refError (Just (ixField, ixStart))

                 -- We parsed the field.
                 Just (x, ptrNext)
                  -> do 
                        -- Update the starting offset for this field,
                        -- for when we read the next column.
                        let !ixStart' = ixStart + F.minusPtr ptrNext ptrStart + 1
                        unsafeWriteBuffer ixsStart ixField ixStart'

                        -- Write the read field value.
                        unsafeWriteBuffer bufOut   ixField x

                        -- Read the next field in the column.
                        loop refError (ixField + 1)
{-# INLINE unsafeUnpacksToBuffer #-}

