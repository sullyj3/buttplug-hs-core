{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Map.Strict     as Map

import qualified Buttplug.Devices    as Dev
-- import           Buttplug.Devices    (Device)
-- import           Buttplug.JSONUtils


import qualified Buttplug            as Butt
import           Buttplug
import           Buttplug.Extra

app :: ButtPlugApp
app = ButtPlugApp { handleDeviceAdded = handleDeviceAdded }
  where
    handleDeviceAdded (DeviceAddedFields id deviceName deviceIdx devMsgs) = 
      case Map.lookup Dev.VibrateCmd devMsgs of
        Just (Dev.MessageAttributes _nMotors) -> vibePulse1s deviceIdx
        Nothing -> pure ()

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runButtPlugWSApp "localhost" 12345 app
