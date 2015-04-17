{-# LANGUAGE RecordWildCards #-}
module SerialComponents ( newSerialHandle
						) where
import System.IO
import System.Serial
import System.Posix.Terminal

newSerialHandle :: IO Handle
newSerialHandle = do
	openSerial dev br ws sb par fc
	where
		dev = "/dev/ttyS0"
		br  = B9600
		ws  = 8
		sb  = One
		par = NoParity
		fc  = NoFlowControl
