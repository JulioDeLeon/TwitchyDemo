{-# LANGUAGE RecordWildCards #-}
module ClientComponents ( ClientName
						, Client
						, Message(Notice,Tell,Broadcast,Command)
						, newClient
						, sendMessage
						) where
import Network
import Control.Concurrent.STM
import System.IO

--type synonyms
type ClientName = String

data Client = Client
	{ clientName     :: ClientName
	, clientHandle   :: Handle
	, clientKicked   :: TVar (Maybe String)
	, clientSendChan :: TChan Message
	}

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
	c <- newTChan
	k <- newTVar Nothing
	return Client { clientName     = name
				  , clientHandle   = handle 
				  , clientSendChan = c
				  , clientKicked   = k
				  } 

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..}  msg =
    writeTChan clientSendChan msg
-- Note: {..} is  record wildcard pattern. To use this in your code, enable RecordWildCards.
--       Notice the Pragma on the top of this file. 


