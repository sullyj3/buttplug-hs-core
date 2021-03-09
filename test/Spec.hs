{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

import           Text.RawString.QQ
import           GHC.Generics
import qualified Data.Map.Strict          as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck    ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text
import           Generic.Random           ( genericArbitraryU )
import           Data.Aeson               ( decode
                                          , eitherDecode
                                          , encode
                                          , ToJSON(..)
                                          , FromJSON(..)
                                          , Options(..)
                                          , SumEncoding(..)
                                          , genericToJSON
                                          , genericParseJSON
                                          )
import           Data.Aeson.Encode.Pretty ( encodePretty )
import qualified Data.Text                as T
import           Data.Text.Encoding       ( decodeUtf8 )
import qualified Data.Text.IO             as T
import           Data.Maybe (isJust)
import           Data.ByteString          ( ByteString )
import qualified Data.ByteString          as BS
import           Data.ByteString.Lazy     ( toStrict )
import           Data.Word                ( Word8 )

import           Buttplug.Core.Message
import           Buttplug.Core.Device
import           Buttplug.Core.Internal.JSONUtils

instance Arbitrary RawData where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary MessageAttributes where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary DeviceMessageType where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary Device where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary ErrorCode where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary Vibrate where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary LinearActuate where
  arbitrary = genericArbitraryU
  shrink = genericShrink
instance Arbitrary Rotate where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Message where
  arbitrary = genericArbitraryU
  shrink = genericShrink

main :: IO ()
main = hspec do
  testButtplug

testButtplug = do
  describe "Message Aeson instances" do
    describe "Ok" do
      let s = [r|{
              "Ok": {
                "Id": 1
                  }
                }|]
          msg = MsgOk 1
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "Error" do
      let s = [r|{
              "Error": {
                "Id": 0,
                "ErrorMessage": "Server received invalid JSON.",
                "ErrorCode": 3
                }
              }|]
          msg = MsgError 0 "Server received invalid JSON." ERROR_MSG
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "Ping" do
      let s = [r|{
                  "Ping": {
                    "Id": 5
                  }
                }|]
          msg = MsgPing 5
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RequestServerInfo" do
      let s = [r|{
                    "RequestServerInfo": {
                      "Id": 1,
                      "ClientName": "Test Client",
                      "MessageVersion": 1
                    }
                  }|]
          msg = MsgRequestServerInfo 1 "Test Client" 1
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg


    describe "ServerInfo" do
      let s = [r|{
                  "ServerInfo": {
                    "Id": 1,
                    "ServerName": "Test Server",
                    "MessageVersion": 1,
                    "MaxPingTime": 100
                  }
                }|]
          msg = MsgServerInfo 1 "Test Server" 1 100
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "StartScanning" do
      let s = [r|{
                    "StartScanning": {
                      "Id": 1
                    }
                  }|]
          msg = MsgStartScanning 1
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "StopScanning" do
      let s = [r|{
                    "StopScanning": {
                      "Id": 1
                    }
                  }|]
          msg = MsgStopScanning 1
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "ScanningFinished" do
      let s = [r|{
                    "ScanningFinished": {
                      "Id": 0
                    }
                  }|]
          msg = MsgScanningFinished 0
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RequestDeviceList" do
      let s = [r|{
                    "RequestDeviceList": {
                      "Id": 1
                    }
                  }|]
          msg = MsgRequestDeviceList 1
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "DeviceList" do
      let s = [r|{
                    "DeviceList": {
                      "Id": 1,
                      "Devices": [
                        {
                          "DeviceName": "TestDevice 1",
                          "DeviceIndex": 0,
                          "DeviceMessages": {
                            "VibrateCmd": { "FeatureCount": 2 },
                            "StopDeviceCmd": {}
                          }
                        },
                        {
                          "DeviceName": "TestDevice 2",
                          "DeviceIndex": 1,
                          "DeviceMessages": {
                            "LinearCmd": { "FeatureCount": 1 },
                            "StopDeviceCmd": {}
                          }
                        }
                      ]
                    }
                  }|]
          msg =
            MsgDeviceList 1
              [ Device "TestDevice 1" 0 $ Map.fromList
                  [ (DevVibrateCmd, MessageAttributes (Just 2) Nothing)
                  , (DevStopDeviceCmd, MessageAttributes Nothing Nothing )]
              , Device "TestDevice 2" 1 $ Map.fromList
                  [ (DevLinearCmd, MessageAttributes (Just 1) Nothing)
                  , (DevStopDeviceCmd, MessageAttributes Nothing Nothing )]
              ]
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "DeviceAdded" do
      let s = [r|{
                    "DeviceAdded": {
                      "Id": 0,
                      "DeviceName": "TestDevice 1",
                      "DeviceIndex": 0,
                      "DeviceMessages": {
                        "VibrateCmd": { "FeatureCount": 2 },
                        "StopDeviceCmd": {}
                      }
                    }
                  }|]
          msg = MsgDeviceAdded 0 "TestDevice 1" 0 $ Map.fromList
            [ (DevVibrateCmd, MessageAttributes (Just 2) Nothing)
            , (DevStopDeviceCmd, MessageAttributes Nothing Nothing )]
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "DeviceRemoved" do
      let s = [r|{
                    "DeviceRemoved": {
                      "Id": 0,
                      "DeviceIndex": 0
                    }
                  }|]
          msg = MsgDeviceRemoved 0 0
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    it "Can decode a ByteString to RawData" $ do
      (decode "[0, 1, 0]" :: Maybe RawData) `shouldBe` Just (RawData $ BS.pack [0,1,0])

    describe "RawWriteCmd" $ do
      let s = [r|{
                    "RawWriteCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Endpoint": "tx",
                      "Data": [0, 1, 0],
                      "WriteWithResponse": false
                    }
                  }|]
          msg = MsgRawWriteCmd 1 0 "tx" (RawData $ BS.pack [0,1,0]) False
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RawReadCmd" $ do
      let s = [r|{
                    "RawReadCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Endpoint": "tx",
                      "ExpectedLength": 0,
                      "WaitForData": false
                    }
                  }|]
          msg = MsgRawReadCmd 1 0 "tx" 0 False
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "a RawReading" $ do
      let s = [r|{
                    "RawReading": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Endpoint": "rx",
                      "Data": [0, 1, 0]
                    }
                  }|]
          msg = MsgRawReading 1 0 "rx" (RawData $ BS.pack [0,1,0])
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RawSubscribeCmd" $ do
      let s = [r|{
                    "RawSubscribeCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Endpoint": "tx"
                    }
                  }|]
          msg = MsgRawSubscribeCmd 1 0 "tx"
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RawUnSubscribeCmd" $ do
      let s = [r|{
                    "RawUnsubscribeCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Endpoint": "tx"
                    }
                  }|]
          msg = MsgRawUnsubscribeCmd 1 0 "tx"
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "StopDeviceCmd" $ do
      let s = [r|{
                    "StopDeviceCmd": {
                      "Id": 1,
                      "DeviceIndex": 0
                    }
                  }|]
          msg = MsgStopDeviceCmd 1 0
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "StopAllDevices" $ do
      let s = [r|{
                    "StopAllDevices": {
                      "Id": 1
                    }
                  }|]
          msg = MsgStopAllDevices 1
      it "Can decode it" do
        decode s `shouldBe` Just msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "VibrateCmd" $ do
      let s = [r|{
                    "VibrateCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Speeds": [
                        {
                          "Index": 0,
                          "Speed": 0.5
                        },
                        {
                          "Index": 1,
                          "Speed": 1.0
                        }
                      ]
                    }
                  }|]
          msg = MsgVibrateCmd 1 0 [ Vibrate 0 0.5, Vibrate 1 1.0 ]
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "LinearCmd" $ do
      let s = [r| {
                    "LinearCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Vectors": [
                        {
                          "Index": 0,
                          "Duration": 500,
                          "Position": 0.3
                        },
                        {
                          "Index": 1,
                          "Duration": 1000,
                          "Position": 0.8
                        }
                      ]
                    }
                  }|]
          msg = MsgLinearCmd 1 0
            [ LinearActuate 0 500 0.3, LinearActuate 1 1000 0.8 ]
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RotateCmd" $ do
      let s = [r| {
                    "RotateCmd": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "Rotations": [
                        {
                          "Index": 0,
                          "Speed": 0.5,
                          "Clockwise": true
                        },
                        {
                          "Index": 1,
                          "Speed": 1.0,
                          "Clockwise": false
                        }
                      ]
                    }
                  }|]
          msg = MsgRotateCmd 1 0 
            [ Rotate 0 0.5 True, Rotate 1 1.0 False ]
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "BatteryLevelCmd" $ do
      let s = [r| {
                    "BatteryLevelCmd": {
                      "Id": 1,
                      "DeviceIndex": 0
                    }
                  }|]
          msg = MsgBatteryLevelCmd 1 0
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "BatteryLevelReading" $ do
      let s = [r| {
                    "BatteryLevelReading": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "BatteryLevel": 0.5
                    }
                  }|]
          msg = MsgBatteryLevelReading 1 0 0.5
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RSSILevelCmd" $ do
      let s = [r| {
                    "RSSILevelCmd": {
                      "Id": 1,
                      "DeviceIndex": 0
                    }
                  }|]
          msg = MsgRSSILevelCmd 1 0
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "RSSILevelReading" $ do
      let s = [r| {
                    "RSSILevelReading": {
                      "Id": 1,
                      "DeviceIndex": 0,
                      "RSSILevel": -40
                    }
                  }|]
          msg = MsgRSSILevelReading 1 0 (-40)
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    describe "all messages" do
      prop "decode is inverse to encode" decodeEncodeInverse
      where
        decodeEncodeInverse :: Message -> Expectation
        decodeEncodeInverse msg = 
          (decode . encode $ msg) `shouldBe` Just msg
