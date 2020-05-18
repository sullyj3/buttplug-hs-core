{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Test.Hspec
import Data.Aeson (decode)
import Data.Maybe (isJust)

import Buttplug

main :: IO ()
main = hspec do
  describe "decode" do
    let non_empty_device_list = "[{\"ServerInfo\":{\"MajorVersion\":0,\"MinorVersion\":5,\"BuildVersion\":5,\"MessageVersion\":1,\"MaxPingTime\":0,\"ServerName\":\"Intiface Server\",\"Id\":1}},{\"DeviceList\":{\"Devices\":[{\"DeviceName\":\"Youou Wand Vibrator\",\"DeviceIndex\":1,\"DeviceMessages\":{\"SingleMotorVibrateCmd\":{},\"VibrateCmd\":{\"FeatureCount\":1},\"StopDeviceCmd\":{}}}],\"Id\":1}},{\"Ok\":{\"Id\":1}}]"
    it "can decode a list of messages with a nonempty device list" $
      (decode non_empty_device_list :: Maybe [Message]) `shouldSatisfy` isJust

