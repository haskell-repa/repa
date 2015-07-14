
module Data.Repa.Store.Prim.BiMap
        ( BiMap
        , connectWithSep

        -- * Conversions
        , keysFwd
        , keysRev

        -- * Loading
        , loadRev
        , loadFwd

        -- * Refection
        , reflect
        , reflectFwd
        , reflectRev)
where
import Data.IORef
import Data.Repa.Convert
import Data.Text                                (Text)
import Data.HashMap.Strict                      (HashMap)
import System.IO.Unsafe
import Data.Repa.Array.Material.Boxed
import qualified Data.HashMap.Strict            as HM
import qualified Data.Repa.Array.Generic        as A
import qualified Data.Repa.Array.Auto.Format    as A
import qualified Data.Repa.Array.Auto.IO        as A


-- | Bi-directional map backed by a flat text file which defines the mapping.
data BiMap a b
        = BiMap
        { biMapPath :: FilePath  -- ^ Path to the external map.
        , _biMapSep :: Char      -- ^ Field separator for file.
        , biMapFwd  :: IORef (Maybe (HashMap a b))
                                 -- ^ Demand-loaded mapping for the forward direction.
        , biMapRev  :: IORef (Maybe (HashMap a b)) 
                                 -- ^ Demand-loaded mapping for the reverse direction.
        }


-- | Connect an external bidirectional map.
-- 
--   The map is defined as a flat text file, with fields separated by the
--   provided character.
--
connectWithSep 
        :: Char                 -- ^ Field separator.
        -> FilePath             -- ^ File that defines the mapping.
        -> IO (BiMap Text Text)

connectWithSep sep path 
 = do   refFwd  <- newIORef Nothing
        refRev  <- newIORef Nothing
        return  $ BiMap path sep refFwd refRev


-- Conversions ------------------------------------------------------------------------------------
-- | Get an array of all keys in the forward direction.
keysFwd :: BiMap Text Text -> A.Array B Text
keysFwd bimap
 = unsafePerformIO
 $ do   Just fwd <- readIORef $ biMapFwd $ loadFwd bimap
        return   $  A.fromList B $ HM.keys fwd


-- | Get an array of all keys in the reverse direction.
keysRev :: BiMap Text Text -> A.Array B Text
keysRev bimap
 = unsafePerformIO
 $ do   Just rev <- readIORef $ biMapRev $ loadRev bimap
        return   $  A.fromList B $ HM.keys rev


-- Loading ----------------------------------------------------------------------------------------
-- | Load the complete forward direction mapping into memory.
loadFwd :: BiMap Text Text -> BiMap Text Text
loadFwd bimap
 = unsafePerformIO
 $ (readIORef $ biMapFwd bimap)
 >>= \case 
        Just{}  
         ->     return bimap

        Nothing
         -> do  arr8      <- A.readFile $ biMapPath bimap
                let !arr  =  A.unpacksFormatLn formatBiMap arr8
                let !hm   =  HM.fromList $ fmap (\(k :*: v :*: ()) -> (k, v)) $ A.toList arr
                writeIORef (biMapFwd bimap) (Just hm)
                return bimap


-- | Load the complete forward direction mapping into memory.
loadRev :: BiMap Text Text -> BiMap Text Text
loadRev bimap
 = unsafePerformIO
 $ (readIORef $ biMapRev bimap)
 >>= \case 
        Just{}  
         ->     return bimap

        Nothing
         -> do  arr8      <- A.readFile $ biMapPath bimap
                let !arr  =  A.unpacksFormatLn formatBiMap arr8
                let !hm   =  HM.fromList $ fmap (\(v :*: k :*: ()) -> (k, v)) $ A.toList arr
                writeIORef (biMapRev bimap) (Just hm)
                return bimap


-- Reflect ----------------------------------------------------------------------------------------
-- | Reflect both directions of a bidirectional map.
--
--   * The first time one of the functions is applied we demand-load the whole
--     table into a strict `HashMap`. 
-- 
--   * A reference to this maybe-loaded `HashMap` is captured in the closure of
--     the produced functions.
--
--   * The on-disk data is not reloaded if it changes.
--
reflect :: BiMap Text Text
        -> ( Text -> Maybe Text
           , Text -> Maybe Text)
reflect bimap
 = ( reflectFwd bimap
   , reflectRev bimap)


-- | Like `reflect`, but only for the forward direction.
reflectFwd :: BiMap Text Text -> (Text -> Maybe Text)
reflectFwd bimap
 = unsafePerformIO
 $ do   Just hm <- readIORef $ biMapFwd $ loadFwd bimap
        return  $ flip HM.lookup hm


-- | Like `reflect`, but only for the reverse direction.
reflectRev :: BiMap Text Text -> (Text -> Maybe Text)
reflectRev bimap
 = unsafePerformIO
 $ do   Just hm <- readIORef $ biMapRev $ loadRev bimap
        return  $  flip HM.lookup hm


-- Format for BiMap data.
formatBiMap
        =   mkSep ','
        $   VarText     -- Datum Id
        :*: VarText     -- Instrument Name.
        :*: ()

