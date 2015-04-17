{-# LANGUAGE RecordWildCards #-}
--exports
module ClientComponents ( ClientName
 						, Client (..)
 						, newClient
 						, Message (..)
						) where
--imports						
import Control.Concurrent.STM
import System.IO



type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k
                }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String
             

