{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import           Control.Monad       (forever)
import           Data.Foldable       (traverse_)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Map.Strict     as Map
import           Control.Concurrent (forkIO, threadDelay)

import qualified Buttplug.Devices    as Dev
-- import           Buttplug.Devices    (Device)
-- import           Buttplug.JSONUtils

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
