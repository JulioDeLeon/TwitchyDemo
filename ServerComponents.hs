{-# LANGUAGE RecordWildCards #-}

module ServerComponents ( Server(..)
						, newServer
						, talk
						, serialHandler
						) where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import Data.Char
import ClientComponents
import SerialComponents

data Server = Server
  { clients      :: TVar (Map ClientName Client)
  , serialHandle :: Handle --This could be switched out for a Serial Manager
  , serialQueue  :: TVar [String]
  }
  
newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  sh <- newSerialHandle 
  q <- newTVarIO []
  return Server { clients = c, serialHandle = sh, serialQueue = q}

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
  

queueMessage :: Server -> String -> STM ()
queueMessage Server{..} msg = do
	modifyTVar serialQueue (\lst -> lst++[msg])

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing     -> return False
    Just client -> sendMessage client msg >> return True

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
     then return ()
     else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case Map.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (Notice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked " ++ who)


talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
      
  hSetBuffering handle LineBuffering
  readName
 where
  readName = do
    hPutStrLn handle "What is your name?"
    name <- hGetLine handle
    if null name
      then readName
      else mask $ \restore -> do       
             ok <- checkAddClient server name handle
             case ok of
               Nothing -> restore $ do  
                  hPrintf handle
                     "The name %s is in use, please choose another\n" name
                  readName
               Just client ->
                  restore (runClient server client) >> removeClient server name

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do client <- newClient name handle
            writeTVar clients $ Map.insert name client clientmap
            broadcast server  $ Notice (name ++ " has connected")
            return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

	--what is join?
  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
      Just reason -> return $
        hPutStrLn clientHandle $ "You have been kicked: " ++ reason
      Nothing -> do
        msg <- readTChan clientSendChan
        return $ do
            continue <- handleMessage serv client msg
            when continue $ server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     Broadcast name msg -> do 
       atomically $ queueMessage server msg
       output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/kick", who] -> do
               atomically $ kick server who clientName
               return True
           "/tell" : who : what -> do
               tell server client who (unwords what)
               return True
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ Broadcast clientName msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
   
   
sendToSerial :: Handle -> String -> IO ()
sendToSerial h s = do 
	if s == "null" 
		then 
			return ()
		else 
			putStrLn $  "sending " ++ s;
			hPutStr h s
   

serialHandler :: Server -> Handle -> IO ()
serialHandler s@Server{..} h = do
	loop
	where 
		loop = do
			threadDelay 500 --milliseconds
			curList <- readTVarIO serialQueue
			atomically $ writeTVar serialQueue []
			mapM_ (\x ->putStrLn $ "value: " ++ x) curList
			msg <- determineComm curList
			--printf " <%s> to serial\n" msg
			sendToSerial h msg
			loop
		
			
determineComm :: [[Char]] -> IO String
determineComm list = do
	if list == []
		then return ("null")
		else do
			printf "L: %d, R: %d, F: %d, B: %d\n" leftC rightC forwardC backC;
			case maximum [leftC, rightC, backC, forwardC] of
				leftC -> return ("left")
				rightC -> return ("right")
				backC -> return ("back")
				forwardC -> return ("forward")
				otherwise -> return ("null")
	where
		leftC = length $ filter (\e-> if (map toLower e) == "left" then True else False) list
		rightC = length $ filter (\e-> if (map toLower e) == "right" then True else False) list
		backC = length $ filter (\e-> if (map toLower e) == "back" then True else False) list
		forwardC = length $ filter (\e-> if (map toLower e) == "forward" then True else False) list
	-- do I have to worry about string sizes?

	
	
	
	
