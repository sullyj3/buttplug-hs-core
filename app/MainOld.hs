{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

module MainOld where


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
import           Extra
import           NonCore

app :: ButtPlugApp
app = ButtPlugApp { handleDeviceAdded = handleDeviceAdded }
  where
    handleDeviceAdded dev@(Device deviceName deviceIdx devMsgs) =
      case Map.lookup Dev.VibrateCmd devMsgs of
        Just (Dev.MessageAttributes _nMotors) -> vibeTease1 dev 1
        Nothing -> pure ()

foo dev = do
  rampdown5s dev
  stopDevice dev

randomInterp :: Device -> ButtPlugM ()
randomInterp dev = do
  start <- liftIO $ randomRIO (0, 1)
  go start

  where go :: Double -> ButtPlugM ()
        go a = do
          b <- liftIO $ randomRIO (0, 1)
          duration <- liftIO $ randomRIO (0.1,1)
          interpolate dev a b duration
          go b

vibeTease1 :: Device -> Double -> ButtPlugM ()
vibeTease1 dev scale = do
  liftIO do putStrLn $ "Starting vibeTease1, scale=" <> show scale
            putStrLn ""
  forever (vibeRampUpPulse dev scale)

tease2 :: Device -> ButtPlugM ()
tease2 dev = for_ [1..] \n -> do
  vibePulse dev (n*0.5) 1
  waitLen <- liftIO $ randomRIO (5,15)
  sleep waitLen

fastThrob :: Device -> ButtPlugM ()
fastThrob dev = do
  forever $ vibeRampUp dev 1 0.1

vibeRampUp :: Device -> Double -> Double -> ButtPlugM ()
vibeRampUp dev scale periodS = do
  
  for_ [1..nSamples] \i ->
    vibrateAllMotors dev (scale * i / nSamples) >> sleep (1/sampleRateHz)
  stopDevice dev
  where
    nSamples = sampleRateHz * periodS

sampleRateHz = 100

vibeRampUpPulse :: Device -> Double -> ButtPlugM ()
vibeRampUpPulse dev scale = do
  vibeRampUp dev scale 0.1

  pulses <- liftIO $ randomRIO (1, 15)
  for_ [1..pulses] \i -> do
    vibePulse dev (i * 0.03) (scale * (0.85 + i/100))
    stopDevice dev <* sleep (i * 0.03)

  sleep $ 0.3 + pulses * 0.065

interpolate :: Device -> Double -> Double -> Double -> ButtPlugM ()
interpolate dev a b duration = do
  for_ [1..nSamples] \s -> do
    let freq = lerp a b (s/nSamples)
    vibrateAllMotors dev freq
    sleep (1/sampleRateHz)
  where
    nSamples = sampleRateHz * duration

rampUp5s dev = interpolate dev 0 1 5
rampdown5s dev = interpolate dev 1 0 5

lerp :: Double -> Double -> Double -> Double
lerp a b t = a + t * (b - a)

--------------------------------------------------------------------------------
main :: IO ()
main = runButtPlugApp (Butt.WebSocketConnector "localhost" 12345) app
