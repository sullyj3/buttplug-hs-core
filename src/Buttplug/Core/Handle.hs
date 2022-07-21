{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Buttplug.Core.Handle
Copyright   : (c) James Sully, 2020-2021
License     : BSD 3-Clause
Maintainer  : sullyj3@gmail.com
Stability   : experimental
Portability : untested

Provides methods of connecting to a Buttplug Server
-}
module Buttplug.Core.Handle where
-- We export Handle with its constructor exposed to facilitate writing implementations for it
-- (eg websockets)
-- A higher level library in the future might reexport it as opaque

import           Data.ByteString              ( ByteString )
import           Buttplug.Core.Message
import           Control.Exception

-- TODO maybe just put this in Buttplug.Core
data Handle = Handle 
  { sendMessages :: [Message] -> IO ()
  , receiveMessages :: IO [Message] }

sendMessage :: Handle -> Message -> IO ()
sendMessage hndl msg = sendMessages hndl [msg]

data ButtplugException = ConnectionFailed String
                       | UnexpectedConnectionClosed
                       | ConnectionClosedNormally
                       | ReceivedInvalidMessage ByteString
                       | OtherConnectorError String
  deriving Show

instance Exception ButtplugException
