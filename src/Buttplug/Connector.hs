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

module Buttplug.Connector where

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

import           Buttplug.Message


-- Connector instances should throw only ConnectorException
class Connector c where
  type Connection c = conn | conn -> c
  -- the protocol allows sending multiple messages simultaneously, wrapped in a JSON array
  sendMessages :: Connection c -> [Message] -> IO ()


  receiveMsgs :: Connection c -> IO [Message] 

  runClient :: c -> (Connection c -> IO a) -> IO a

sendMessage :: forall c. Connector c => Connection c -> Message -> IO ()
sendMessage conn msg = sendMessages @c conn [msg]

data WebSocketConnector = InsecureWebSocketConnector { insecureWSConnectorHost :: String
                                                     , insecureWSConnectorPort :: Int }
                        | SecureWebSocketConnector { secureWSConnectorHost :: String
                                                   , secureWSConnectorPort :: PortNumber
                                                   , secureWSBypassCertVerify :: Bool }

-- I'm not incredibly psyched about this design, but it's not immediately
-- obvious to me how to improve it.
data ConnectorException = ConnectionFailed String
                        | UnexpectedConnectionClosed
                        | ConnectionClosedNormally
                        | InvalidMessage ByteString
                        | OtherConnectorError String
  deriving Show

instance Exception ConnectorException


--------------------

instance Connector WebSocketConnector where
  type Connection WebSocketConnector = WS.Connection

  sendMessages :: Connection WebSocketConnector -> [Message] -> IO ()
  sendMessages wsCon msgs = handle handleWSConnException $
    WS.sendTextData wsCon (encode msgs)

  receiveMsgs :: Connection WebSocketConnector -> IO [Message]
  receiveMsgs wsCon = handle handleWSConnException $ do
    received <- WS.receiveData wsCon
    case decode $ fromStrict received :: Maybe [Message] of
      Just msgs -> pure msgs
      Nothing -> throwIO $ InvalidMessage received

  -- TODO: handle socket does not exist connection refused
  runClient connector client =
    handle handleSockConnFailed $ handle handleWSConnFailed $
      withSocketsDo $ case connector of
        InsecureWebSocketConnector host port ->
           WS.runClient host port "/" client
        SecureWebSocketConnector host port bypassCertVerify ->
          if bypassCertVerify
            then do
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

handleWSConnFailed :: WS.HandshakeException -> IO a
handleWSConnFailed e = throwIO (ConnectionFailed $ show e)

handleSockConnFailed :: IOError -> IO a
handleSockConnFailed e
  | isDoesNotExistError e = throwIO (ConnectionFailed $ show e)
  | otherwise           = throwIO e

handleWSConnException :: WS.ConnectionException -> IO a
handleWSConnException = \case
  WS.ConnectionClosed    -> throwIO UnexpectedConnectionClosed
  WS.CloseRequest 1000 _ -> throwIO ConnectionClosedNormally
  e                      -> throwIO $ OtherConnectorError (show e)
