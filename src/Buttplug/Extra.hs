module Buttplug.Extra where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class

import Buttplug

vibePulse1s :: Int -> ButtPlugM ()
vibePulse1s deviceIdx = do
  vibrateOnlyMotor deviceIdx 1
  liftIO $ threadDelay 1000000
  vibrateOnlyMotor deviceIdx (1/255)

