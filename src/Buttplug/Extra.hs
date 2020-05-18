module Buttplug.Extra where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class

import Buttplug
import Buttplug.Devices

vibePulseFullSpeed :: Device -> Int -> ButtPlugM ()
vibePulseFullSpeed dev microsnds = vibePulse dev microsnds 1

vibePulse :: Device -> Int -> Double -> ButtPlugM ()
vibePulse dev microsnds speed = do
  vibrateOnlyMotor dev speed
  liftIO $ threadDelay microsnds
  stopDevice dev
