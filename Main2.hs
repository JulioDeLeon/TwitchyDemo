module Main (main) where
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Control.Exception
import Network
import System.Environment
import System.Serial.Manager
import System.IO
import Text.Printf

port :: Int
port = 4000

main = withSocketsDo $ do
	sock <- listenOn (PortNumber (fromIntegral port))
	printf "Listening on port %d \n" port
	forever $ do 
		(handle, host, portn) <- accept sock
		printf "Accepted connection from %s: %s\n" host (show portn)
		forkIO (talk handle)

	--intialize STM for Clients
	--initalize STM for Serial Comms.

	--forkIO for Serial Comms 
	
talk :: Handle -> IO()
talk h = do
	hSetBuffering h LineBuffering
	loop
	where
		loop = do 
		line <- hGetLine h
		if line == "end" || line == "exit"
				then hPutStrLn h ("Thank you for using the Haskell doubling service.")	>> hClose h
			else hPutStrLn h (show (2 * (read line :: Integer))) >> loop
 
						


{-
serialHandle :: STM -> IO ()
serialHandle will take in the STM serial comms. SerialHandle will continously 
read from serialComms, then write that command to a serial port.
-}
