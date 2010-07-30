
module Config
	( Config(..)
	, configDefault
	, verb)
where
	
data Config
	= Config
	{ configVerbose		:: Bool 
	, configMaxThreads	:: Int }
	
configDefault 
	= Config
	{ configVerbose		= True
	, configMaxThreads	= 8 }
	
	
verb :: Config -> String -> IO ()
verb config str
 	| configVerbose config
	= putStrLn str

	| otherwise
	= return ()
	