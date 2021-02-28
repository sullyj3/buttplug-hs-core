{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Main where


import           Data.List           (find)
import qualified Data.Text.IO        as T
import           Data.Foldable       (for_)
import           Control.Monad       (forever)
import           Control.Exception   (handle)
import qualified Network.WebSockets  as WS

import           Buttplug


main :: IO ()
main = do
  let connector =
        InsecureWebSocketConnector { insecureWSConnectorHost = "localhost"
                                   , insecureWSConnectorPort = 12345 }
  let clientName = "Haskell-example-buttplug-client"

  runClient connector \con -> do
    putStrLn "Beginning handshake..."
    sendMessage con
      RequestServerInfo { msgId = 1
                        , msgClientName = clientName
                        , msgMessageVersion = clientMessageVersion
                        }
    reply <- receiveMsgs con
    case find isServerInfo reply of
      Nothing -> putStrLn "Did not receive handshake response"
      Just (ServerInfo 1 servName _msgVersion _maxPingTime) -> handle
        handler
        do T.putStrLn $ "Successfully connected to server \"" <> servName <> "\"!"
           putStrLn "Requesting device scan"
           sendMessage con $ StartScanning 2
           putStrLn "(receiving messages)"
           forever do arr <- receiveMsgs con
                      for_ arr print

  where
    -- TODO: this should be a buttplug error, client shouldn't care about websockets
    handler :: WS.ConnectionException -> IO ()
    handler = \case
      WS.ConnectionClosed -> putStrLn "Server closed the connection unexpectedly"
      WS.CloseRequest c _ -> putStrLn $
        "Server closed the connection: status code " <> show c
      e -> do putStrLn "websocketError:"
              print e
