{-# LANGUAGE BangPatterns #-}
module Internal.Load where
import Page
import Progress
import qualified Data.ByteString.Lazy.Char8     as BL
import qualified Data.Vector                    as V
import qualified Data.Vector.Mutable            as VM
import qualified Data.Vector.Unboxed            as U


-- | Add rank contributions due to forward-links to a ranks vector.
loadPages
        :: FilePath             -- ^ Pages file.
        -> IO (V.Vector Page)

loadPages filePath 
 = {-# SCC "loadPages" #-}
   do   bs              <- BL.readFile filePath

        let bufSize     = 1000
        mvec            <- VM.new bufSize
        vec'            <- go mvec bufSize 0 0 (BL.lines bs)

        return vec'

 where  go !mvec !bufSize !ixLine !ixPage ls
         -- We've read all the lines.
         -- Slice out the pages we read from the buffer.
         | []       <- ls
         = do   printPosition True "  lines read : " 10000 ixLine
                V.freeze (VM.slice 0 ixPage mvec)

         -- Handle a new line from the file.
         | l : rest <- ls
         = do   -- Print how far along we are.
                printPosition False "  lines read : " 10000 ixLine

                -- Parse the page and add it to the buffer.
                let Just page   = parsePage l
                addPage mvec bufSize (ixLine + 1) ixPage rest page

         | otherwise
         = error "loadPages failed"


        addPage !mvec !bufSize !ixLine !ixPage ls !page
         -- We're out of space in the buffer.
         -- Copy the pointers to the page data to a bigger one.
         | ixPage >= bufSize
         = do   mvec'   <- VM.grow mvec bufSize
                addPage mvec' (2 * bufSize) ixLine ixPage ls page

         -- Ok, we read the page we were expecting.
         | pageId page == ixPage
         = do   
                -- Add it to the buffer.
                VM.write mvec ixPage page

                -- Read some more pages.
                go mvec bufSize ixLine (ixPage + 1) ls

         -- The page id was higher than what we were expecting.
         -- We've skipped over some page with no out-links that was
         -- not mentioned in the source file.
         | pageId page >= ixPage
         = do   
                -- Add a place-holder page to the buffer.
                let !page' = Page ixPage U.empty
                VM.write mvec ixPage page'

                -- Move to the next page id.
                addPage mvec bufSize ixLine (ixPage + 1) ls page

         -- If the page id read from the file is less than what
         -- we were expecting then the links file isn't sorted.
         | otherwise
         = error "loadPages: page ids in links file are not monotonically increasing"

