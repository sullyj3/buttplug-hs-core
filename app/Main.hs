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

import           Control.Monad.IO.Class

import qualified Buttplug            as Butt
import           Buttplug

app :: ButtPlugM ()
app = do
    con <- Butt.getConnection
    liftIO $ putStrLn "Connected!"

    recvTID <- liftIO $ forkIO $ forever $ runButtPlugM con handleReceivedData

    let reqServerInfo = RequestServerInfo $
          RequestServerInfoFields { id = 1
                                  , clientName = "haskell"
                                  , messageVersion = clientMessageVersion }
        reqDeviceList = RequestDeviceList $ RequestDeviceListFields 1
        startScanning = StartScanning $ StartScanningFields 1

    -- WS.sendTextData con (encode [reqServerInfo, reqDeviceList, startScanning])
    Butt.sendMessages [reqServerInfo, reqDeviceList, startScanning]

    liftIO $ T.putStrLn "Press enter to exit"

    _ <- liftIO getLine
    Butt.close

  where
    handleReceivedData :: ButtPlugM ()
    handleReceivedData = do
      liftIO $ T.putStrLn "Listening for messages from server"
      forever $ Butt.receiveMsgs >>= handleMsgs

    handleMsgs :: [Message] -> ButtPlugM ()
    handleMsgs msgs = do
      liftIO $ T.putStrLn "Handling received messages\n"
      traverse_ handleMsg msgs

    --handleMsg :: ButtPlugConnection -> Message -> IO ()
    handleMsg :: Message -> ButtPlugM ()
    handleMsg = \case
      (DeviceAdded (DeviceAddedFields id name idx devMsgs)) ->
        case Map.lookup Dev.VibrateCmd devMsgs of
          Just (Dev.MessageAttributes _nMotors) -> vibePulse1s idx
          Nothing -> pure ()
      msg -> liftIO$ T.putStrLn $ "Received unhandled message: " <> (T.pack $ show msg)

vibePulse1s :: Int -> ButtPlugM ()
vibePulse1s deviceIdx = do
  Butt.sendMessage $ VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 1 ])
  liftIO $ threadDelay 1000000
  Butt.sendMessage $ VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 (1/255) ])

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runClient "localhost" 12345 app
