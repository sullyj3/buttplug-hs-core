{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where


import           Data.List           (find)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Foldable       (for_)

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device(..))
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
     Just (ServerInfo 1 servName msgVersion maxPingTime) -> do
       T.putStrLn $ "Successfully connected to server \"" <> servName <> "\"!"
       putStrLn "Requesting device scan"
       sendMessage con $ StartScanning 2
       putStrLn "(receiving messages)"
       loop con

  where
    loop con = do
      arr <- receiveMsgs con
      for_ arr print
      loop con
