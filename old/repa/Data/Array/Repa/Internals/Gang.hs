{-# LANGUAGE CPP #-}

-- | Gang Primitives.
--   Based on DPH code by Roman Leshchinskiy
--
--   Gang primitives.
--
#define TRACE_GANG 0

module Data.Array.Repa.Internals.Gang
	( Gang, seqGang, forkGang, gangSize, gangIO, gangST, traceGang, traceGangST
	, theGang)
where
import GHC.IO
import GHC.ST
import GHC.Conc                  (forkOn)

import Control.Concurrent.MVar
import Control.Exception         (assert)

import Control.Monad             (zipWithM, zipWithM_)
import GHC.Conc			(numCapabilities)
import System.IO

#if TRACE_GANG
import GHC.Exts                  (traceEvent)
import System.Time ( ClockTime(..), getClockTime )
#endif

-- TheGang ----------------------------------------------------------------------------------------
-- | The gang is shared by all computations.
theGang :: Gang
{-# NOINLINE theGang #-}
theGang = unsafePerformIO $ forkGang numCapabilities


-- Requests ---------------------------------------------------------------------------------------
-- | The 'Req' type encapsulates work requests for individual members of a gang.
data Req
	-- | Instruct the worker to run the given action then signal it's done
	--   by writing to the MVar.
	= ReqDo	       (Int -> IO ()) (MVar ())

	-- | Tell the worker that we're shutting the gang down. The worker should
        --   signal that it's received the request down by writing to the MVar
        --   before returning to its caller (forkGang)
	| ReqShutdown  (MVar ())


-- | Create a new request for the given action.
newReq :: (Int -> IO ()) -> IO Req
newReq p
 = do	mv	<- newEmptyMVar
	return	$ ReqDo p mv


-- | Block until a thread request has been executed.
--   NOTE: only one thread can wait for the request.
waitReq :: Req -> IO ()
waitReq req
 = case req of
	ReqDo     _ varDone	-> takeMVar varDone
	ReqShutdown varDone	-> takeMVar varDone


-- Gang ------------------------------------------------------------------------------------------
-- | A 'Gang' is a group of threads which execute arbitrary work requests.
--   To get the gang to do work, write Req-uest values to its MVars
data Gang
	= Gang !Int           -- Number of 'Gang' threads
               [MVar Req]     -- One 'MVar' per thread
               (MVar Bool)    -- Indicates whether the 'Gang' is busy


instance Show Gang where
  showsPrec p (Gang n _ _)
	= showString "<<"
        . showsPrec p n
        . showString " threads>>"


-- | A sequential gang has no threads.
seqGang :: Gang -> Gang
seqGang (Gang n _ mv) = Gang n [] mv


-- | The worker thread of a 'Gang'.
--   The threads blocks on the MVar waiting for a work request.
gangWorker :: Int -> MVar Req -> IO ()
gangWorker threadId varReq
 = do	traceGang $ "Worker " ++ show threadId ++ " waiting for request."
	req	<- takeMVar varReq

	case req of
	 ReqDo action varDone
	  -> do	traceGang $ "Worker " ++ show threadId ++ " begin"
		start 	<- getGangTime
		action threadId
		end 	<- getGangTime
		traceGang $ "Worker " ++ show threadId ++ " end (" ++ diffTime start end ++ ")"

		putMVar varDone ()
		gangWorker threadId varReq

	 ReqShutdown varDone
	  -> do	traceGang $ "Worker " ++ show threadId ++ " shutting down."
		putMVar varDone ()


-- | Finaliser for worker threads.
--   We want to shutdown the corresponding thread when it's MVar becomes unreachable.
--     Without this Repa programs can complain about "Blocked indefinitely on an MVar"
--     because worker threads are still blocked on the request MVars when the program ends.
--     Whether the finalizer is called or not is very racey. It happens about 1 in 10 runs
--     when for the repa-edgedetect benchmark, and less often with the others.
--
--   We're relying on the comment in System.Mem.Weak that says
--    "If there are no other threads to run, the runtime system will check for runnable
--     finalizers before declaring the system to be deadlocked."
--
--   If we were creating and destroying the gang cleanly we wouldn't need this, but theGang
--     is created with a top-level unsafePerformIO. Hacks beget hacks beget hacks...
--
finaliseWorker :: MVar Req -> IO ()
finaliseWorker varReq
 = do	varDone <- newEmptyMVar
	putMVar varReq (ReqShutdown varDone)
	takeMVar varDone
	return ()


-- | Fork a 'Gang' with the given number of threads (at least 1).
forkGang :: Int -> IO Gang
forkGang n
 = assert (n > 0)
 $ do
	-- Create the vars we'll use to issue work requests.
	mvs	<- sequence . replicate n $ newEmptyMVar

	-- Add finalisers so we can shut the workers down cleanly if they become unreachable.
	mapM_ (\var -> addMVarFinalizer var (finaliseWorker var)) mvs

	-- Create all the worker threads
	zipWithM_ forkOn [0..]
		$ zipWith gangWorker [0 .. n-1] mvs

	-- The gang is currently idle.
	busy	<- newMVar False

	return $ Gang n mvs busy


-- | The number of threads in the 'Gang'.
gangSize :: Gang -> Int
gangSize (Gang n _ _) = n


-- | Issue work requests for the 'Gang' and wait until they have been executed.
--   If the gang is already busy then just run the action in the
--   requesting thread.
--
--   TODO: We might want to print a configurable warning that this is happening.
--
gangIO	:: Gang
	-> (Int -> IO ())
	-> IO ()

{-# NOINLINE gangIO #-}
gangIO (Gang n mvs busy) p
 = do	traceGang   "gangIO: issuing work requests (SEQ_IF_GANG_BUSY)"
	b <- swapMVar busy True

	traceGang $ "gangIO: gang is currently " ++ (if b then "busy" else "idle")
	if b
	 then do
		hPutStr stderr
		 $ unlines	[ "Data.Array.Repa: Performing nested parallel computation sequentially."
				, "  You've probably called the 'force' function while another instance was"
				, "  already running. This can happen if the second version was suspended due"
				, "  to lazy evaluation. Use 'deepSeqArray' to ensure that each array is fully"
				, "  evaluated before you 'force' the next one."
				, "" ]

		mapM_ p [0 .. n-1]

	 else do
		parIO n mvs p
		_ <- swapMVar busy False
		return ()


-- | Issue some requests to the worker threads and wait for them to complete.
parIO 	:: Int			-- ^ Number of threads in the gang.
	-> [MVar Req]		-- ^ Request vars for worker threads.
	-> (Int -> IO ())	-- ^ Action to run in all the workers, it's given the ix of
				--   the particular worker thread it's running on.
	-> IO ()

parIO n mvs p
 = do	traceGang "parIO: begin"

	start 	<- getGangTime
	reqs	<- sequence . replicate n $ newReq p

	traceGang "parIO: issuing requests"
	_ <- zipWithM putMVar mvs reqs

	traceGang "parIO: waiting for requests to complete"
	mapM_ waitReq reqs
	end 	<- getGangTime

	traceGang $ "parIO: end " ++ diffTime start end


-- | Same as 'gangIO' but in the 'ST' monad.
gangST :: Gang -> (Int -> ST s ()) -> ST s ()
gangST g p = unsafeIOToST . gangIO g $ unsafeSTToIO . p


-- Tracing ----------------------------------------------------------------------------------------
#if TRACE_GANG
getGangTime :: IO Integer
getGangTime
 = do	TOD sec pico <- getClockTime
	return (pico + sec * 1000000000000)

diffTime :: Integer -> Integer -> String
diffTime x y = show (y-x)

traceGang :: String -> IO ()
traceGang s
 = do	t <- getGangTime
	traceEvent $ show t ++ " @ " ++ s

#else
getGangTime :: IO ()
getGangTime = return ()

diffTime :: () -> () -> String
diffTime _ _ = ""

traceGang :: String -> IO ()
traceGang _ = return ()

#endif

traceGangST :: String -> ST s ()
traceGangST s = unsafeIOToST (traceGang s)

