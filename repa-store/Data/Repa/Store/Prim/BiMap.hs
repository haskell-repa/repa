
module Data.Repa.Store.Prim.BiMap
        ( BiMap  (..)
        , connectWithSep
        , reflect
        , reflectFwd
        , reflectRev)
where
import Data.IORef
import Data.Repa.Convert
import Data.Text                                (Text)
import Data.HashMap.Strict                      (HashMap)
import System.IO.Unsafe
import qualified Data.HashMap.Strict            as HM
import qualified Data.Repa.Array.Auto           as A
import qualified Data.Repa.Array.Auto.Format    as A
import qualified Data.Repa.Array.Auto.IO        as A


-- | Bi-directional map backed by a flat text file which defines the mapping.
data BiMap a b
        = BiMap
        { biMapPath :: FilePath  -- ^ Path to the external map.
        , biMapSep  :: Char      -- ^ Field separator for file.
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
reflect
        :: BiMap Text Text
        -> ( Text -> Maybe Text
           , Text -> Maybe Text)
reflect bimap
 = ( reflectFwd bimap
   , reflectRev bimap)


-- | Like `reflect` but only for the forward direction.
reflectFwd :: BiMap Text Text -> (Text -> Maybe Text)
reflectFwd bimap
 =   unsafePerformIO
 $   (readIORef $ biMapFwd bimap)
 >>= \case 
        Just hm
         ->     return  $ flip HM.lookup hm

        Nothing
         -> do  arr8     <- A.readFile  $ biMapPath bimap
                let !arr =  A.unpacksFormatLn formatBiMap arr8
                let !hm  =  HM.fromList $ fmap (\(k :*: v :*: ()) -> (k, v)) $ A.toList arr
                return   $  flip HM.lookup hm


-- | Like `reflect` but only for the reverse direction.
reflectRev :: BiMap Text Text -> (Text -> Maybe Text)
reflectRev bimap
 =   unsafePerformIO
 $   (readIORef $ biMapFwd bimap)
 >>= \case 
        Just hm
         ->     return  $ flip HM.lookup hm

        Nothing
         -> do  arr8     <- A.readFile  $ biMapPath bimap
                let !arr =  A.unpacksFormatLn formatBiMap arr8
                let !hm  =  HM.fromList $ fmap (\(v :*: k :*: ()) -> (k, v)) $ A.toList arr
                return   $  flip HM.lookup hm


-- Format for BiMap data.
formatBiMap
        =   mkSep ','
        $   VarText     -- Datum Id
        :*: VarText     -- Instrument Name.
        :*: ()

