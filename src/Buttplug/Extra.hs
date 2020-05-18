module Buttplug.Extra where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class

import Buttplug

vibePulseFullSpeed :: Int -> Int -> ButtPlugM ()
vibePulseFullSpeed deviceIdx microsnds = vibePulse deviceIdx microsnds 1

vibePulse :: Int -> Int -> Double -> ButtPlugM ()
vibePulse deviceIdx microsnds speed = do
  vibrateOnlyMotor deviceIdx speed
  liftIO $ threadDelay microsnds
  vibrateOnlyMotor deviceIdx (1/255) -- todo: change once Stop is fixed in server for youou
