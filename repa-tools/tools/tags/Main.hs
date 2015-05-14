
module Main where
import Config
import Data.Maybe
import System.FilePath
import System.Directory
import Control.Monad
import qualified Text.HTML.TagSoup      as TS
import qualified System.Environment     as S


---------------------------------------------------------------------------------------------------
main :: IO ()
main
 = do   args    <- S.getArgs
        config  <- parseArgs args configZero
        mapM_ (unpack config) $ configInFiles config


---------------------------------------------------------------------------------------------------
-- | Name of a tag.
type Name       = String

-- | Key and value of some attribute.
type Attr       = (String, String)

-- | XML context consits of tags that we've entered into, as well as the
--   key attribute and value of that tag, if there was one.
type Context    
        =  [(String, Maybe (String, String))]

-- | Parse the given XML file and unpack it into files of tuples.
unpack config file
 = do   
        hasDir   <- doesDirectoryExist dir
        when (not hasDir)
         $ error $ "repa-tags: output directory " ++ show dir ++ " does not exist."

        ss       <- readFile file
        let tags =  TS.parseTags ss
        eat [] tags

 where 
        dir     = fromMaybe "." (configOutDir config)

        eat _ctx []
         = do   return ()


        -- Treat self closing tags as rows in a table whose name
        -- is defined by the surrounding XML context.
        eat ctx (TS.TagOpen name1 attrs : TS.TagClose name2 : moar)
         | name1 == name2
         = do   aggregateRow dir ctx name1 (attrs ++ configInsertAttrs config)
                eat ctx moar

        -- Skip over XML header opening tags.
        eat ctx (TS.TagOpen name _attrs : moar)
         | name == "?xml"       
         = eat ctx moar

        eat ctx (TS.TagOpen name attrs : moar)
         = case attrs of
            attr1 : _
              -> do
                aggregateRow dir ctx name (attrs ++ configInsertAttrs config)
                eat ((name, Just attr1) : ctx) moar

            _ -> do
                eat ((name, Nothing)    : ctx) moar

        -- Handle closing tags.
        eat ctx (TS.TagClose name : moar)
         = case ctx of
            (name', _) : ctx'
             -- Close matching tag.
             | name == name'
             -> eat ctx' moar

             -- Close unmatched tag.
             | otherwise
             -> error $ unlines 
                [ "repa-tags: Unmatched closing tag " ++ show name
                , "  in context: " ++ show ctx ]


            [] 
             -> error $ unlines
                [ "repa-tags: Unmatched closing tag " ++ show name
                , "  in top-level context." ]

        -- Print text tags.
        eat ctx (TS.TagText tt : moar)
         = do   putStrLn $ "* Text " ++ show tt
                eat ctx moar

        -- Print comment tags.
        eat ctx (TS.TagComment tt : moar)
         = do   putStrLn $ "* Comment " ++ show tt
                eat ctx moar

        -- Print warnings tags.
        eat ctx (TS.TagWarning tt : moar)
         = do   
                putStrLn $ "* Warning " ++ show tt
                eat ctx moar

        -- Drop position tags.
        eat ctx (TS.TagPosition _ _ : moar)
         =      eat ctx moar


---------------------------------------------------------------------------------------------------
-- | Aggreagate the given row into its associated table.
aggregateRow  
        :: FilePath             -- ^ Output directory.
        -> Context              -- ^ XML context.
        -> String               -- ^ Row tag name.
        -> [Attr]               -- ^ Row attributes.
        -> IO ()

aggregateRow dir ctx0 name0 attrs0
 = do   let (dirParts, fileName, attrs)
                = slurpTarget ctx0 name0 attrs0

        writeRow dir dirParts fileName attrs


-- | Append the given row to the given table.
writeRow 
        :: FilePath             -- ^ Output directory.
        -> [String]             -- ^ Table directory.
        -> String               -- ^ Table name.
        -> [(String, String)]   -- ^ Row attributes.
        -> IO ()

writeRow dir parts name attrs
 = do   let fileName 
                = foldl (\n p -> n ++ "#" ++ p) 
                        "part" parts 
                ++ "#" ++ name ++ ".lists"

        appendFile (dir </> fileName) (show attrs ++ "\n")


---------------------------------------------------------------------------------------------------
-- | Given an XML context along with the name and attributes of a leaf
--   tag in that context, produce an appropriate table name and encoding
--   of the data as a row.
slurpTarget 
        :: Context              -- ^ Current XML context.
        -> String               -- ^ Name of Tag.
        -> [Attr]               -- ^ Attributes of Tag.
        -> ([String], String, [Attr])
                                -- ^ Table directory, table name, and row attributes.

slurpTarget ctx name0 attrs0
 = go (reverse ctx) [] name0 attrs0
 where
        go [] path name attrs
         =      (reverse path, name, attrs)

        go ((tName, Nothing)  : ctx') path name attrs
         = go   ctx' 
                (tName : path)
                name 
                attrs

        go ((tName, Just (aName, val)) : ctx') path name attrs
         = go   ctx'
                path
                (tName ++ "_" ++ name) 
                ((aName, val) : attrs)

