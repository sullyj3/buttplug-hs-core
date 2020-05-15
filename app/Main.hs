{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import           GHC.Generics
import           Control.Monad       (forever, unless)
import           Data.Foldable       (traverse_)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Text.Encoding  (decodeUtf8)
import qualified Network.WebSockets  as WS
import           Data.Aeson          ( ToJSON(..)
                                     , FromJSON(..)
                                     , (.=)
                                     , Options(..)
                                     , Value(..)
                                     , object
                                     , genericToJSON
                                     , genericParseJSON
                                     , defaultOptions
                                     , encode
                                     , decode)
import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
import           Data.HashMap.Strict as HMap
import           Control.Concurrent (forkIO, threadDelay)
import           Data.ByteString.Lazy (fromStrict)

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device)
import           Buttplug.JSONUtils

import qualified Buttplug            as Butt
import           Buttplug

app :: WS.ClientApp ()
app con = do
    putStrLn "Connected!"

    recvTID <- forkIO $ forever $ handleReceivedData con

    let reqServerInfo = RequestServerInfo $
          RequestServerInfoFields { id = 1
                                  , clientName = "haskell"
                                  , messageVersion = clientMessageVersion }
        reqDeviceList = RequestDeviceList $ RequestDeviceListFields 1
        startScanning = StartScanning $ StartScanningFields 1

    WS.sendTextData con (encode [reqServerInfo, reqDeviceList, startScanning])

    T.putStrLn "Press enter to exit"

    _ <- getLine
    WS.sendClose con ("Bye!" :: Text)

  where
    handleReceivedData :: WS.Connection -> IO ()
    handleReceivedData con = T.putStrLn "Listening for messages from server" >> forever do
      received <- WS.receiveData con
      T.putStrLn $ "Server sent: " <> decodeUtf8 received
      case decode $ fromStrict received :: Maybe [Message] of
        Just msgs -> handleMsgs msgs con
        Nothing -> T.putStrLn "Couldn't decode the message from the server"

    handleMsgs :: [Message] -> WS.Connection -> IO ()
    handleMsgs msgs con = do
      T.putStrLn "Handling received messages\n"
      traverse_ (flip handleMsg con) msgs

    handleMsg :: Message -> WS.Connection -> IO ()
    handleMsg (DeviceAdded (DeviceAddedFields id name idx devMsgs)) con =
      case Map.lookup Dev.VibrateCmd devMsgs of
        Just (Dev.MessageAttributes nVibes) -> do
          vt1TID <- forkIO $ vibePulse1s idx con
          pure ()
        Nothing -> pure ()
      
    handleMsg msg con = T.putStrLn $ "Received unhandled message: " <> (T.pack $ show msg)

vibePulse1s :: Int -> WS.Connection -> IO ()
vibePulse1s deviceIdx con = do
  let msg1 = VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 1 ])
  WS.sendTextData con (encode [msg1])
  threadDelay 1000000
  let msg2 = VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 (1/255) ])
  WS.sendTextData con (encode [msg2])

--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "localhost" 12345 "/" app
