
-- | Gang Primitives.
module Data.Repa.Eval.Gang
        (Gang, forkGang, gangSize, gangIO, gangST)     
where
import GHC.IO
import GHC.ST
import GHC.Conc                 (forkOn)
import Control.Concurrent.MVar
import Control.Exception        (assert)
import Control.Monad
import System.IO
import GHC.Exts


-- Requests -------------------------------------------------------------------
-- | The 'Req' type encapsulates work requests for individual members of a gang.
data Req
        -- | Instruct the worker to run the given action.
        = ReqDo        (Int# -> IO ())

        -- | Tell the worker that we're shutting the gang down.
        --   The worker should signal that it's receieved the request by
        --   writing to its result var before returning to the caller (forkGang).
        | ReqShutdown


-- Gang -----------------------------------------------------------------------
-- | A 'Gang' is a group of threads that execute arbitrary work requests.
data Gang
        = Gang 
        { -- | Number of threads in the gang.
          _gangThreads           :: Int#

          -- | Workers listen for requests on these vars.
        , _gangRequestVars       :: [MVar Req]     

          -- | Workers put their results in these vars.
        , _gangResultVars        :: [MVar ()] 

          -- | Indicates that the gang is busy.
        , _gangBusy              :: MVar Bool
        } 

instance Show Gang where
  showsPrec p (Gang n _ _ _)
        = showString "<<"
        . showsPrec p (I# n)
        . showString " threads>>"


-- | O(1). Yield the number of threads in the 'Gang'.
gangSize :: Gang -> Int#
gangSize (Gang n _ _ _) 
        = n
{-# NOINLINE gangSize #-}


-- | Fork a 'Gang' with the given number of threads (at least 1).
forkGang :: Int -> IO Gang
forkGang !n@(I# n_)
 = assert (n > 0)
 $ do
        -- Create the vars we'll use to issue work requests.
        mvsRequest     <- sequence $ replicate n $ newEmptyMVar

        -- Create the vars we'll use to signal that threads are done.
        mvsDone        <- sequence $ replicate n $ newEmptyMVar

        -- Add finalisers so we can shut the workers down cleanly if they
        -- become unreachable.
        zipWithM_ (\varReq varDone 
                        -> mkWeakMVar varReq (finaliseWorker varReq varDone)) 
                mvsRequest
                mvsDone

        -- Create all the worker threads
        zipWithM_ forkOn [0..]
                $ zipWith3 (\(I# i) -> gangWorker i)
                        [0 .. n - 1] mvsRequest mvsDone

        -- The gang is currently idle.
        busy   <- newMVar False

        return $ Gang n_ mvsRequest mvsDone busy
{-# NOINLINE forkGang #-}


-- | The worker thread of a 'Gang'.
--   The threads blocks on the MVar waiting for a work request.
gangWorker :: Int# -> MVar Req -> MVar () -> IO ()
gangWorker threadId varRequest varDone
 = do   
        -- Wait for a request 
        req     <- takeMVar varRequest

        case req of
         ReqDo action
          -> do -- Run the action we were given.
                action threadId

                -- Signal that the action is complete.
                putMVar varDone ()

                -- Wait for more requests.
                gangWorker threadId varRequest varDone

         ReqShutdown
          ->    putMVar varDone ()
{-# NOINLINE gangWorker #-}


-- | Finaliser for worker threads.
--   We want to shutdown the corresponding thread when it's MVar becomes
--   unreachable.
--   Without this Repa programs can complain about "Blocked indefinitely
--   on an MVar" because worker threads are still blocked on the request
--   MVars when the program ends. Whether the finalizer is called or not
--   is very racey. It happens about 1 in 10 runs when for the
--   repa-edgedetect benchmark, and less often with the others.
--
--   We're relying on the comment in System.Mem.Weak that says
--    "If there are no other threads to run, the runtime system will
--     check for runnablefinalizers before declaring the system to be
--     deadlocked."
--
--   If we were creating and destroying the gang cleanly we wouldn't need
--     this, but theGang is created with a top-level unsafePerformIO.
--     Hacks beget hacks beget hacks...
--
finaliseWorker :: MVar Req -> MVar () -> IO ()
finaliseWorker varReq varDone 
 = do   putMVar varReq ReqShutdown
        takeMVar varDone
        return ()
{-# NOINLINE finaliseWorker #-}


-- | Issue work requests for the 'Gang' and wait until they complete.
--
--   If the gang is already busy then print a warning to `stderr` and just
--   run the actions sequentially in the requesting thread.
gangIO  :: Gang
        -> (Int# -> IO ())
        -> IO ()

gangIO gang@(Gang _ _ _ busy) action
 = do   b <- swapMVar busy True
        if b
         then do
                seqIO gang action

         else do
                parIO gang action
                _ <- swapMVar busy False
                return ()
{-# NOINLINE gangIO #-}


-- | Run an action on the gang sequentially.
seqIO   :: Gang -> (Int# -> IO ()) -> IO ()
seqIO (Gang n _ _ _) action
 = do   hPutStr stderr
         $ unlines
         [ "Data.Array.Repa.Bulk.Par: Performing nested parallel computation sequentially."
         , "  Something is trying to run a compuation on a gang that is already busy.     "
         , "  You've probably used a Repa 'computeP', 'foldP' or similar function while   "
         , "  another instance was already running. This can happen if you've passed a    "
         , "  parallel worker function to a combinator like 'map', or some parallel       "
         , "  compuation was suspended via lazy evaluation. Try using `seq` to ensure that"
         , "  each array is fully evaluated before computing the next one.                "
         , "" ]

        mapM_ (\(I# i) -> action i) [0 .. (I# n) - 1]
{-# NOINLINE seqIO #-}


-- | Run an action on the gang in parallel.
parIO   :: Gang -> (Int# -> IO ()) -> IO ()
parIO (Gang _ mvsRequest mvsResult _) action
 = do   
        -- Send requests to all the threads.
        mapM_ (\v -> putMVar v (ReqDo action)) mvsRequest

        -- Wait for all the requests to complete.
        mapM_ takeMVar mvsResult
{-# NOINLINE parIO #-}


-- | Same as 'gangIO' but in the 'ST' monad.
gangST :: Gang -> (Int# -> ST s ()) -> ST s ()
gangST g p 
        = unsafeIOToST $ gangIO g (\i -> unsafeSTToIO $ p i)
{-# NOINLINE gangST #-}


