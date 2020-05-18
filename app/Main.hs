module Main where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Map.Strict     as Map

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device(..))

import qualified Buttplug            as Butt
import           Buttplug
import           Buttplug.Extra

app :: ButtPlugApp
app = ButtPlugApp { handleDeviceAdded = handleDeviceAdded }
  where
    handleDeviceAdded (Device deviceName deviceIdx devMsgs) = 
      case Map.lookup Dev.VibrateCmd devMsgs of
        Just (Dev.MessageAttributes _nMotors) -> vibePulse1s deviceIdx
        Nothing -> pure ()

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runButtPlugApp (Butt.WebSocketConnector "localhost" 12345) app
