{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

module Main where


import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Map.Strict     as Map
import           Control.Monad.IO.Class
import           System.Random
import           Control.Concurrent  (threadDelay)
import           Data.Foldable       (for_)
import           Control.Monad       (replicateM_, forever)

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device(..))

import qualified Buttplug            as Butt
import           Buttplug
import           Buttplug.Extra

app :: ButtPlugApp
app = ButtPlugApp { handleDeviceAdded = handleDeviceAdded }
  where
    handleDeviceAdded dev@(Device deviceName deviceIdx devMsgs) =
      case Map.lookup Dev.VibrateCmd devMsgs of
        Just (Dev.MessageAttributes _nMotors) -> vibeTease1 dev 1
        Nothing -> pure ()

vibeTease1 :: Device -> Double -> ButtPlugM ()
vibeTease1 dev scale = do
  liftIO do putStrLn $ "Starting vibeTease1, scale=" <> show scale
            putStrLn ""
  forever (vibeRampUpPulse dev scale)

vibeRampUpPulse :: Device -> Double -> ButtPlugM ()
vibeRampUpPulse dev scale = do
  for_ [1..50] \i ->
    vibrateAllMotors dev (scale * i / 50) >> sleep 0.05
  stopDevice dev

  pulses <- liftIO $ randomRIO (1, 15)
  for_ [1..pulses] \i -> do
    vibePulse dev (i * 0.03) (scale * (0.85 + i/100))
    stopDevice dev <* sleep (i * 0.03)

  sleep $ 0.3 + pulses * 0.065

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runButtPlugApp (Butt.WebSocketConnector "localhost" 12345) app
