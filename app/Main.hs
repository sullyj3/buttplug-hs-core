{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

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

app :: ButtPlugApp
app = ButtPlugApp { handleDeviceAdded = handleDeviceAdded }
  where
    handleDeviceAdded (DeviceAddedFields id deviceName deviceIdx devMsgs) = 
      case Map.lookup Dev.VibrateCmd devMsgs of
        Just (Dev.MessageAttributes _nMotors) -> vibePulse1s deviceIdx
        Nothing -> pure ()

vibePulse1s :: Int -> ButtPlugM ()
vibePulse1s deviceIdx = do
  con <- getConnection
  liftIO $ Butt.sendMessage con $ VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 1 ])
  liftIO $ threadDelay 1000000
  liftIO $ Butt.sendMessage con $ VibrateCmd (VibrateCmdFields 1 deviceIdx [ MotorVibrate 0 (1/255) ])

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runButtPlugWSApp "localhost" 12345 app
