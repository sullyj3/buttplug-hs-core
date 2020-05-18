module Buttplug.Extra where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class

import Buttplug
import Buttplug.Devices

vibePulseFullSpeed :: Device -> Double -> ButtPlugM ()
vibePulseFullSpeed dev seconds = vibePulse dev seconds 1

vibePulse :: Device -> Double -> Double -> ButtPlugM ()
vibePulse dev seconds speed = do
  vibrateOnlyMotor dev speed <* sleep seconds
  stopDevice dev

sleep :: MonadIO m => Double -> m ()
sleep seconds = liftIO $ threadDelay $ round $ 1000000 * seconds
