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

--User Defined Modules
--import ClientComponents
import ServerComponents

port :: Int
port = 4000

main :: IO()
main = withSocketsDo $ do
	server <- newServer
	sock <- listenOn (PortNumber (fromIntegral port))
	printf "Listening on %d \n" port 
	--start serial Handle here
	
	--forever accept connections
	forever $ do
		(handle, host, port) <- accept sock
		printf "Accepted connection from %s: %s\n" host (show port)
		forkIO (talk handle server)


