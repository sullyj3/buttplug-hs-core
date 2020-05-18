{-# LANGUAGE BlockArguments #-}

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

vibePulse1s :: Device -> ButtPlugM ()
vibePulse1s dev = vibePulseFullSpeed dev 1000000

randomNumber :: Device -> ButtPlugM ()
randomNumber dev = do
  n <- liftIO (randomRIO (0,10) :: IO Int)
  liftIO $ putStrLn $ "n is " <> show n
  pure ()

vibeTease1 :: Device -> Double -> ButtPlugM ()
vibeTease1 dev scale = do
  liftIO do putStrLn $ "Starting vibeTease1, scale=" <> show scale
            putStrLn ""
  forever (vibeRampUpPulse dev scale)

vibeRampUpPulse :: Device -> Double -> ButtPlugM ()
vibeRampUpPulse dev scale = do
  for_ [1..50] \i -> do vibrateOnlyMotor dev $ scale * i / 50
                        liftIO $ threadDelay 50000
  stopDevice dev

  pulses <- liftIO $ randomRIO (1, 15)
  for_ [1..pulses] \i -> do
    vibrateOnlyMotor dev $ scale * (0.85 + i/100)
    sleep $ i * 0.03
    stopDevice dev
    sleep $ i * 0.03

  sleep $ 0.3 + pulses * 0.065

--------------------------------------------------------------------------------
main :: IO ()
main = Butt.runButtPlugApp (Butt.WebSocketConnector "localhost" 12345) app
