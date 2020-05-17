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

app :: ButtPlugConnection -> IO ()
app con = do
    putStrLn "Connected!"

    recvTID <- forkIO $ forever $ handleReceivedData con

    let reqServerInfo = RequestServerInfo $
          RequestServerInfoFields { id = 1
                                  , clientName = "haskell"
                                  , messageVersion = clientMessageVersion }
        reqDeviceList = RequestDeviceList $ RequestDeviceListFields 1
        startScanning = StartScanning $ StartScanningFields 1

    -- WS.sendTextData con (encode [reqServerInfo, reqDeviceList, startScanning])
    Butt.sendMessages con [reqServerInfo, reqDeviceList, startScanning]

    T.putStrLn "Press enter to exit"

    _ <- getLine
    Butt.close con

  where
    handleReceivedData :: ButtPlugConnection -> IO ()
    handleReceivedData con = do
      T.putStrLn "Listening for messages from server"
      forever $ Butt.receiveMsgs con >>= handleMsgs con

    handleMsgs :: ButtPlugConnection -> [Message] -> IO ()
    handleMsgs con msgs = do
      T.putStrLn "Handling received messages\n"
      traverse_ (handleMsg con) msgs

    handleMsg :: ButtPlugConnection -> Message -> IO ()
    handleMsg con = \case
      (DeviceAdded (DeviceAddedFields id name idx devMsgs)) ->
        case Map.lookup Dev.VibrateCmd devMsgs of
          Just (Dev.MessageAttributes _nMotors) -> do
            vt1TID <- forkIO $ vibePulse1s con idx
            pure ()
          Nothing -> pure ()
      msg -> T.putStrLn $ "Received unhandled message: " <> (T.pack $ show msg)

vibePulse1s :: ButtPlugConnection -> Int -> IO ()
vibePulse1s con deviceIdx = do
  Butt.sendMessage con $ VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 1 ])
  threadDelay 1000000
  Butt.sendMessage con $ VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 (1/255) ])

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runClient "localhost" 12345 app
