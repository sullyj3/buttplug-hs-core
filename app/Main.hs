{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where


import qualified Data.Text.IO             as T
import           Data.Foldable            (for_)
import           Control.Monad            (forever)
import           Control.Exception        (handle)
import qualified Network.WebSockets       as WS
import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent       (threadDelay)

import           Buttplug


main :: IO ()
main = do
  -- A connector represents a method of connecting to a buttplug server, and
  -- contains all of the necessary information required to connect.
  let connector =
        InsecureWebSocketConnector { insecureWSConnectorHost = "localhost"
                                   , insecureWSConnectorPort = 12345 }
  let clientName = "Haskell-example-buttplug-client"

  -- runClient is responsible for establishing and closing the connection
  -- we pass it a function which takes a connection and returns an IO action
  -- which will make use of that connection to send and receive buttplug messages
  handle handler $ runClient connector \con -> do
    putStrLn "Beginning handshake..."

    -- A buttplug handshake involves sending the server a RequestServerInfo message.
    -- we use the sendMessage function to send messages
    -- The server will reply with a ServerInfo message.
    -- see https://buttplug-spec.docs.buttplug.io/architecture.html#stages
    -- for details and diagrams
    sendMessage con
      RequestServerInfo { msgId = 1
                        , msgClientName = clientName
                        , msgMessageVersion = clientMessageVersion
                        }

    -- we receive messages using the receiveMsgs function
    receiveMsgs con >>= \case
      [ServerInfo 1 servName msgVersion maxPingTime] -> do
        T.putStrLn $ "Successfully connected to server \"" <> servName <> "\"!"
        putStrLn $ "Message version: " <> show msgVersion <>
                   "\nMax ping time (ms): " <> show maxPingTime
        -- once we have successfully connected to the server, we ask it to
        -- begin scanning for devices. 
        putStrLn "Requesting device scan"
        sendMessage con $ StartScanning 2

        concurrently_ (receiveAndPrintMsgs con)
                      (pingServer maxPingTime con)
      -- this case would indicate a server bug, it's just here for completeness
      _ -> putStrLn "Did not receive expected handshake response"

  where
    -- TODO: this should be a buttplug error, client shouldn't care about websockets
    handler :: WS.ConnectionException -> IO ()
    handler = \case
      WS.ConnectionClosed -> putStrLn "Server closed the connection unexpectedly"
      WS.CloseRequest c _ -> putStrLn $
        "Server closed the connection: status code " <> show c
      e -> do putStrLn "websocketError:"
              print e

    -- we now print out any further messages the server sends us, until it
    -- disconnects. The first thing we should see is an "Ok Id=2" in
    -- response to our request to start scanning for devices.
    -- Additionally, the server will send us a message any time a device
    -- connects or disconnects. If the server specified a ping timeout, there
    -- will also be an Ok response for each of our pings
    receiveAndPrintMsgs con = do 
      putStrLn "(receiving messages)"
      forever do arr <- receiveMsgs con
                 for_ arr print

    -- if the server's maxPingTime is set to a value other than 0, we need to
    -- ping it regularly, or it will disconnect us. We ping at twice the
    -- specified rate to leave ourselves plenty of room
    pingServer maxPingTime con = case maxPingTime of
      0 -> pure ()
      n -> forever do
        sendMessage con (Ping 1)
        threadDelay (n * 1000 `div` 2)

