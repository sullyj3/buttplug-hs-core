module Buttplug.Extra where

import           UnliftIO.Concurrent (threadDelay)
import           Control.Monad.IO.Class
import           UnliftIO.Async

import Buttplug
import Buttplug.Devices

vibePulseFullSpeed :: Device -> Double -> ButtPlugM ()
vibePulseFullSpeed dev seconds = vibePulse dev seconds 1

vibePulse :: Device -> Double -> Double -> ButtPlugM ()
vibePulse dev seconds speed = do
  vibrateAllMotors dev speed <* sleep seconds
  stopDevice dev

sleep :: MonadIO m => Double -> m ()
sleep seconds = threadDelay $ round $ 1000000 * seconds
