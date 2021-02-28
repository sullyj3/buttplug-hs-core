{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}

module Buttplug.Connector where

import           Data.ByteString.Lazy         ( fromStrict, toStrict )
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


-- TODO: think about the best way to handle errors
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

instance Connector WebSocketConnector where
  type Connection WebSocketConnector = WS.Connection

  sendMessages :: Connection WebSocketConnector -> [Message] -> IO ()
  sendMessages wsCon msgs = WS.sendTextData wsCon (encode msgs)

  receiveMsgs wsCon = do
    received <- WS.receiveData wsCon
    case decode $ fromStrict received :: Maybe [Message] of
      Just msgs -> pure msgs
      Nothing -> error "Couldn't decode the message from the server"

  runClient connector client = withSocketsDo $ case connector of
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

