{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Data.List           (find)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device(..))
import           Buttplug


main :: IO ()
main = do
  let connector = WebSocketConnector "localhost" 12345
  let clientName = "Haskell-example-client"

  runClient connector \con -> do
    putStrLn "Beginning handshake..."
    sendMessage con
      RequestServerInfo { msgId = 1
                        , msgClientName = clientName
                        , msgMessageVersion = clientMessageVersion
                        }
    reply <- receiveMsgs con
    case find isServerInfo reply of
      Just (ServerInfo 1 servName msgVersion maxPingTime) -> do
        T.putStrLn $ "Successfully connected to server \"" <> servName <> "\"!"
      Nothing -> putStrLn "Did not receive handshake response"
