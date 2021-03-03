{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

import           Text.RawString.QQ
import           GHC.Generics
import qualified Data.Map.Strict          as Map
import           Test.Hspec
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
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             as T
import           Data.Maybe (isJust)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.ByteString.Lazy     (toStrict)
import           Data.Word                    ( Word8 )

import           Buttplug.Message
import           Buttplug.Device (Device(..), MessageAttributes(..))
import qualified Buttplug.Device as Dev
import           Buttplug.Internal.JSONUtils

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
          msg = Ok 1
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
          msg = Error 0 "Server received invalid JSON." ERROR_MSG
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
          msg = Ping 5
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
          msg = RequestServerInfo 1 "Test Client" 1
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
          msg = ServerInfo 1 "Test Server" 1 100
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
          msg = StartScanning 1
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
          msg = StopScanning 1
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
          msg = ScanningFinished 0
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
          msg = RequestDeviceList 1
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
            DeviceList 1
              [ Device "TestDevice 1" 0 $ Map.fromList
                  [ (Dev.VibrateCmd, MessageAttributes (Just 2) Nothing)
                  , (Dev.StopDeviceCmd, MessageAttributes Nothing Nothing )]
              , Device "TestDevice 2" 1 $ Map.fromList
                  [ (Dev.LinearCmd, MessageAttributes (Just 1) Nothing)
                  , (Dev.StopDeviceCmd, MessageAttributes Nothing Nothing )]
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
          msg = DeviceAdded 0 "TestDevice 1" 0 $ Map.fromList
            [ (Dev.VibrateCmd, MessageAttributes (Just 2) Nothing)
            , (Dev.StopDeviceCmd, MessageAttributes Nothing Nothing )]
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
          msg = DeviceRemoved 0 0
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
          msg = RawWriteCmd 1 0 "tx" (RawData $ BS.pack [0,1,0]) False
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
          msg = RawReadCmd 1 0 "tx" 0 False
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
          msg = RawReading 1 0 "rx" (RawData $ BS.pack [0,1,0])
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
          msg = RawSubscribeCmd 1 0 "tx"
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
          msg = RawUnsubscribeCmd 1 0 "tx"
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
          msg = StopDeviceCmd 1 0
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
          msg = StopAllDevices 1
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
          msg = VibrateCmd 1 0 [ Vibrate 0 0.5, Vibrate 1 1.0 ]
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
          msg = LinearCmd 1 0
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
          msg = RotateCmd 1 0 
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
          msg = BatteryLevelCmd 1 0
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
          msg = BatteryLevelReading 1 0 0.5
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
          msg = RSSILevelCmd 1 0
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
          msg = RSSILevelReading 1 0 (-40)
      it "Can decode it" do
        eitherDecode s `shouldBe` Right msg
      it "Can encode it" do
        decode (encode msg) `shouldBe` Just msg

    -- TODO
    -- - quickcheck that decode . encode = id
