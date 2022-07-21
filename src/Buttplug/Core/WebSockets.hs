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
module Buttplug.Core.WebSockets
  ( runClient
  , Connector
  )
where

import           Control.Exception
import           System.IO.Error              ( isDoesNotExistError )
import qualified Data.ByteString.Lazy         as LBS
import           Data.ByteString              ( ByteString )
import qualified Network.WebSockets           as WS
import           Network.Socket               ( withSocketsDo )
import           Data.Aeson                   ( encode
                                              , decode )

import           Buttplug.Core.Message
import           Buttplug.Core.Handle

-- | Convert 'WS.HandshakeException' into 'ConnectionFailed'
handleWSConnFailed :: WS.HandshakeException -> IO a
handleWSConnFailed e = throwIO (ConnectionFailed $ show e)

-- | Convert socket connection issues into 'ConnectionFailed'
handleSockConnFailed :: IOError -> IO a
handleSockConnFailed e
  | isDoesNotExistError e = throwIO (ConnectionFailed $ show e)
  | otherwise             = throwIO e

-- | Convert websocket specific connection exceptions into 'ButtplugException'
handleWSConnException :: WS.ConnectionException -> IO a
handleWSConnException e = case e of
  WS.ConnectionClosed    -> throwIO UnexpectedConnectionClosed
  WS.CloseRequest 1000 _ -> throwIO ConnectionClosedNormally
  e                      -> throwIO $ OtherConnectorError (show e)

runClient :: Connector -> (Handle -> IO a) -> IO a
runClient (Connector host port) io =
  handle handleSockConnFailed $ handle handleWSConnFailed $ withSocketsDo $
    WS.runClient host port "/" $ \wsCon -> io (wsHandle wsCon)

wsHandle :: WS.Connection -> Handle
wsHandle wsCon = Handle { sendMessages = sendMessages, receiveMessages = receiveMessages }
  where
    sendMessages msgs = handle handleWSConnException $
      WS.sendTextData wsCon (encode msgs)

    receiveMessages = handle handleWSConnException $ do
      received <- WS.receiveData wsCon
      case decode $ LBS.fromStrict received :: Maybe [Message] of
        Just msgs -> pure msgs
        Nothing -> throwIO $ ReceivedInvalidMessage received


data Connector = Connector
  { wsConnectorHost :: String
  , wsConnectorPort :: Int }

