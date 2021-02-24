{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}

import           GHC.Generics
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Data.Aeson ( decode
                            , encode
                            , ToJSON(..)
                            , FromJSON(..)
                            , Options(..)
                            , SumEncoding(..)
                            , genericToJSON
                            , genericParseJSON
                            )
import           Data.Maybe (isJust)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Word                    ( Word8 )

import           Buttplug.Internal
import           Buttplug.Devices
import qualified Buttplug.Devices as Dev
import           Buttplug.Internal.JSONUtils

main :: IO ()
main = hspec do
  testButtplug

testButtplug = do
  describe "decode" do
    let non_empty_device_list = "[{\"ServerInfo\":{\"MessageVersion\":1,\"MaxPingTime\":0,\"ServerName\":\"Intiface Server\",\"Id\":1}},{\"DeviceList\":{\"Devices\":[{\"DeviceName\":\"Youou Wand Vibrator\",\"DeviceIndex\":1,\"DeviceMessages\":{\"SingleMotorVibrateCmd\":{},\"VibrateCmd\":{\"FeatureCount\":1},\"StopDeviceCmd\":{}}}],\"Id\":1}},{\"Ok\":{\"Id\":1}}]"

    it "Can decode a simple list of messages with a nonempty device list" $
      (decode non_empty_device_list :: Maybe [Message]) `shouldBe` Just expectedNonemptyDeviceFields

    -- should the string instead be "[0, 1, 0]"? Waiting for a reply on discord
    it "Can decode a ByteString" $ do
      (decode "[0, 1, 0]" :: Maybe RawData) `shouldBe` Just (RawData $ BS.pack [0,1,0])

    it "Can decode a RawReading" $ do
      let readingStr = "[ { \"RawReading\": { \"Id\": 1, \"DeviceIndex\": 0, \"Endpoint\": \"rx\", \"Data\": [0, 1, 0] } } ]"
          expectedReading = [RawReading 1 0 "rx" (RawData $ BS.pack [0,1,0])]
      (decode readingStr :: Maybe [Message]) `shouldBe` Just expectedReading
      

-- [{"ServerInfo":{"MajorVersion":0,"MinorVersion":5,"BuildVersion":5,"MessageVersion":1,"MaxPingTime":0,"ServerName":"Intiface Server","Id":1}},{"DeviceList":{"Devices":[{"DeviceName":"Youou Wand Vibrator","DeviceIndex":1,"DeviceMessages":{"SingleMotorVibrateCmd":{},"VibrateCmd":{"FeatureCount":1},"StopDeviceCmd":{}}}],"Id":1}},{"Ok":{"Id":1}}]

expectedNonemptyDeviceFields =
  [ ServerInfo
      { msgMessageVersion = 1
      , msgMaxPingTime = 0
      , msgServerName = "Intiface Server"
      , msgId = 1 }
  , DeviceList
      { msgId = 1
      , msgDevices = [ Device
                         { deviceName = "Youou Wand Vibrator"
                         , deviceIndex = 1
                         , deviceMessages =
                             Map.fromList [ (Dev.SingleMotorVibrateCmd, MessageAttributes Nothing)
                                          , (Dev.VibrateCmd, MessageAttributes $ Just 1)
                                          , (Dev.StopDeviceCmd, MessageAttributes Nothing)
                                          ]
                         }
                  ]

      }
  , Ok { msgId = 1 }
  ]

