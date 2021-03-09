{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Buttplug.Core.Connector
Copyright   : (c) James Sully, 2020-2021
License     : BSD 3-Clause
Maintainer  : sullyj3@gmail.com
Stability   : experimental
Portability : untested

Provides methods of connecting to a Buttplug Server
-}
module Buttplug.Core.Connector where

import           Control.Exception
import           System.IO.Error              ( isDoesNotExistError )
import           Data.ByteString.Lazy         ( fromStrict, toStrict )
import           Data.ByteString              ( ByteString )
import qualified Network.WebSockets           as WS
import           Network.WebSockets.Stream    ( makeStream )
import qualified Wuss
import           Network.Connection           ( TLSSettings(..)
                                              , ConnectionParams(..)
                                              , initConnectionContext
                                              , connectTo
                                              , connectionGetChunk
                                              , connectionPut )
import           Network.Socket               ( withSocketsDo, PortNumber )
import           Data.Aeson                   ( encode
                                              , decode )

import           Buttplug.Core.Message

-- TODO currently websocket connector runclient blocks indefinitely if host
-- exists but port is unavailable. Need to think about providing an API to
-- allow for time limits

-- | Abstracts over methods of connecting to a buttplug server. The connector 
-- contains all the information necessary for establishing a connection.
class Connector c where
  -- | A Connector determines a unique connection type that is used for 
  -- communication.
  type Connection c = conn | conn -> c

  -- | Main entry point for communicating with the Buttplug server. Establish a
  -- connection to the server and pass the connection handle to the 
  -- continuation.
  runClient :: c -> (Connection c -> IO a) -> IO a

  -- | Send 'Message's to the server. In the Buttplug protocol, all messages 
  -- are wrapped in a JSON array (here a Haskell list) to facilitate sending 
  -- multiple messages simultaneously. Use 'sendMessage' to send a single 
  -- message.
  sendMessages :: Connection c -> [Message] -> IO ()

  -- | receive 'Message's from the server
  receiveMsgs :: Connection c -> IO [Message]

-- | Send the server a single 'Message'
sendMessage :: forall c. Connector c => Connection c -> Message -> IO ()
sendMessage conn msg = sendMessages @c conn [msg]

-- | Connect to the buttplug server using websockets
data WebSocketConnector =
    InsecureWebSocketConnector { insecureWSConnectorHost :: String
                               , insecureWSConnectorPort :: Int }
  | SecureWebSocketConnector { secureWSConnectorHost :: String
                             , secureWSConnectorPort :: PortNumber
                             , secureWSBypassCertVerify :: Bool }

-- I'm not incredibly psyched about this design, but it's not immediately
-- obvious to me how to improve it.

-- | An exception type abstracting over the exceptions that might arise in the
-- course of communication with the buttplug server. 'Connector' instances in
-- general should throw these rather than Exceptions specific to the connection
-- type.
data ConnectorException = ConnectionFailed String
                        | UnexpectedConnectionClosed
                        | ConnectionClosedNormally
                        | ReceivedInvalidMessage ByteString
                        | OtherConnectorError String
  deriving Show

instance Exception ConnectorException


instance Connector WebSocketConnector where
  type Connection WebSocketConnector = WS.Connection

  sendMessages :: WS.Connection -> [Message] -> IO ()
  sendMessages wsCon msgs = handle handleWSConnException $
    WS.sendTextData wsCon (encode msgs)

  receiveMsgs :: WS.Connection -> IO [Message]
  receiveMsgs wsCon = handle handleWSConnException $ do
    received <- WS.receiveData wsCon
    case decode $ fromStrict received :: Maybe [Message] of
      Just msgs -> pure msgs
      Nothing -> throwIO $ ReceivedInvalidMessage received

  runClient :: WebSocketConnector -> (WS.Connection -> IO a) -> IO a
  runClient connector client =
    handle handleSockConnFailed $ handle handleWSConnFailed $
      withSocketsDo $ case connector of
        InsecureWebSocketConnector host port ->
           WS.runClient host port "/" client
        SecureWebSocketConnector host port bypassCertVerify ->
          if bypassCertVerify
            then do
              -- adapted from https://hackage.haskell.org/package/wuss-1.1.18/docs/Wuss.html#v:runSecureClientWith
              let options = WS.defaultConnectionOptions
              let headers = []
              let tlsSettings = TLSSettingsSimple
                    -- This is the important setting.
                    { settingDisableCertificateValidation = True
                    , settingDisableSession = False
                    , settingUseServerName = False
                    }
              let connectionParams = ConnectionParams
                    { connectionHostname = host
                    , connectionPort = port
                    , connectionUseSecure = Just tlsSettings
                    , connectionUseSocks = Nothing
                    }

              context <- initConnectionContext
              connection <- connectTo context connectionParams
              stream <- makeStream
                  (fmap Just (connectionGetChunk connection))
                  (maybe (return ()) (connectionPut connection . toStrict))
              WS.runClientWithStream stream host "/" options headers client
            else Wuss.runSecureClient host port "/" client

--         --
-- Private --
--         --

-- | Convert 'WS.HandshakeException' into 'ConnectionFailed'
handleWSConnFailed :: WS.HandshakeException -> IO a
handleWSConnFailed e = throwIO (ConnectionFailed $ show e)

-- | Convert socket connection issues into 'ConnectionFailed'
handleSockConnFailed :: IOError -> IO a
handleSockConnFailed e
  | isDoesNotExistError e = throwIO (ConnectionFailed $ show e)
  | otherwise             = throwIO e

-- | Convert websocket specific connection exceptions into 'ConnectorException'
handleWSConnException :: WS.ConnectionException -> IO a
handleWSConnException = \case
  WS.ConnectionClosed    -> throwIO UnexpectedConnectionClosed
  WS.CloseRequest 1000 _ -> throwIO ConnectionClosedNormally
  e                      -> throwIO $ OtherConnectorError (show e)
