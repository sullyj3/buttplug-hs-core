{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}

{- |
Module      : Buttplug.Core.Message
Copyright   : (c) James Sully, 2020-2021
License     : BSD 3-Clause
Maintainer  : sullyj3@gmail.com
Stability   : experimental
Portability : untested

Contains the Message type, representing Buttplug protocol messages
(<https://buttplug-spec.docs.buttplug.io/messages.html>)
-}
module Buttplug.Core.Message where

import           Data.Foldable                ( asum )
import           Data.Word                    ( Word32 )
import           Data.Text                    ( Text )
import           Data.ByteString              ( ByteString )
import qualified Data.ByteString              as BS
import           Data.Aeson                   ( ToJSON(..)
                                              , FromJSON(..)
                                              , object
                                              , (.=)
                                              , withObject
                                              , (.:)
                                              )
import           Data.Map.Strict              ( Map )

import qualified Buttplug.Core.Device         as Dev
import           Buttplug.Core.Device         ( Device(..) )


-- | The version of the Buttplug message protocol that the client speaks.
-- (currently version 2)
clientMessageVersion :: Word32
clientMessageVersion = 2
------------------------------------------------


-- | Errors from the server, used in the Error message.
--
-- (<https://buttplug-spec.docs.buttplug.io/status.html#error>)
data ErrorCode = ERROR_UNKNOWN  -- ^ An unknown error occurred. 
               | ERROR_INIT     -- ^ Handshake did not succeed.
               | ERROR_PING     -- ^ A ping was not sent in the expected time.
               | ERROR_MSG      -- ^ A message parsing or permission error occurred.
               | ERROR_DEVICE   -- ^ A command sent to a device returned an error.
               deriving (Enum, Show, Eq)


-- TODO these should probably convert with Word not Int
-- | Parse an 'Int' to an 'ErrorCode'
errCodeFromInt :: Int -> Maybe ErrorCode
errCodeFromInt = \case
  0 -> Just ERROR_UNKNOWN
  1 -> Just ERROR_INIT
  2 -> Just ERROR_PING
  3 -> Just ERROR_MSG
  4 -> Just ERROR_DEVICE
  _ -> Nothing


-- | Convert an 'ErrorCode' to an 'Int'
fromErrCode :: ErrorCode -> Int
fromErrCode = fromIntegral . fromEnum


instance ToJSON ErrorCode where
  toJSON = toJSON . fromErrCode


instance FromJSON ErrorCode where
  parseJSON v = do
    m <- errCodeFromInt <$> parseJSON v
    case m of
      Nothing -> fail "Error code should be an int"
      Just e -> pure e

-- Circumvents the fact that Aeson doesn't
-- have bytestring encoding/decoding in genericToJSON and genericParseJSON

-- | Used for the Raw* messages.
newtype RawData = RawData ByteString
  deriving (Show, Eq)


instance ToJSON RawData where
  toJSON (RawData bs) = toJSON $ BS.unpack bs


instance FromJSON RawData where
  parseJSON j = RawData . BS.pack <$> parseJSON j

-- | Used in VibrateCmd to specify the speed of the motor at the given index
data Vibrate = Vibrate { vibrateIndex :: Word32
                       , vibrateSpeed :: Double
                       }
  deriving (Show, Eq)


instance ToJSON Vibrate where
  toJSON vibrate = object
    [ "Index" .= vibrateIndex vibrate,
      "Speed" .= vibrateSpeed vibrate
    ]

instance FromJSON Vibrate where
  parseJSON = withObject "Vibrate" $ \o ->
    Vibrate
      <$> o .: "Index"
      <*> o .: "Speed"

-- | Used in RotateCmd to specify the speed and direction of rotation of the
-- motor at the given index
data Rotate = Rotate
  { rotateIndex :: Word32
  , rotateSpeed :: Double
  , rotateClockwise :: Bool
  }
  deriving (Show, Eq)

instance ToJSON Rotate where
  toJSON rotate = object
    [ "Index" .= rotateIndex rotate,
      "Speed" .= rotateSpeed rotate,
      "Clockwise" .= rotateClockwise rotate
    ]

instance FromJSON Rotate where
  parseJSON = withObject "Rotate" $ \o ->
    Rotate
      <$> o .: "Index"
      <*> o .: "Speed"
      <*> o .: "Clockwise"

-- | Used in LinearCmd to specify how to move the linear actuator at the given
-- index
data LinearActuate = LinearActuate
  { linActIndex :: Word32
  , linActDuration :: Word32
  , linActPosition :: Double
  }
  deriving (Show, Eq)


instance ToJSON LinearActuate where
  toJSON linearActuate = object
    [ "Index" .= linActIndex linearActuate,
      "Duration" .= linActDuration linearActuate,
      "Position" .= linActPosition linearActuate
    ]

instance FromJSON LinearActuate where
  parseJSON = withObject "LinearActuate" $ \o ->
    LinearActuate
      <$> o .: "Index"
      <*> o .: "Duration"
      <*> o .: "Position"

-- TODO technically Ids should be Word32, since the maximum id is 4294967295.
-- Not sure whether this applies to other unsigned fields, should find out

-- | The type of Buttplug protocol messages. See
-- (<https://buttplug-spec.docs.buttplug.io/messages.html>) for the protocol
-- specification and an explanation of the purpose of each message.
data Message =
               -- status messages
               MsgOk { msgId :: Word32 }
             | MsgError { msgId :: Word32
                        , msgErrorMessage :: Text
                        , msgErrorCode :: ErrorCode
                        }
             | MsgPing { msgId :: Word32 }
               -- handshake messages
             | MsgRequestServerInfo { msgId :: Word32
                                    , msgClientName :: Text
                                    , msgMessageVersion :: Word32
                                    }
             | MsgServerInfo { msgId :: Word32
                             , msgServerName :: Text
                             , msgMessageVersion :: Word32
                             , msgMaxPingTime :: Word32
                             }
               -- enumeration messages
             | MsgStartScanning { msgId :: Word32 }
             | MsgStopScanning { msgId :: Word32 }
             | MsgScanningFinished { msgId :: Word32 }
             | MsgRequestDeviceList { msgId :: Word32 }
             | MsgDeviceList { msgId :: Word32
                             , msgDevices :: [ Device ]
                             }
             | MsgDeviceAdded { msgId :: Word32
                              , msgDeviceName :: Text
                              , msgDeviceIndex :: Word32
                              , msgDeviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
                              }
             | MsgDeviceRemoved { msgId :: Word32
                                , msgDeviceIndex :: Word32
                                }
               -- raw device messages
             | MsgRawWriteCmd { msgId :: Word32
                              , msgDeviceIndex :: Word32
                              , msgEndpoint :: Text
                              , msgData :: RawData
                              , msgWriteWithResponse :: Bool }
             | MsgRawReadCmd { msgId :: Word32
                             , msgDeviceIndex :: Word32
                             , msgEndpoint :: Text
                             , msgExpectedLength :: Word32
                             , msgWaitForData :: Bool }
             | MsgRawReading { msgId :: Word32
                             , msgDeviceIndex :: Word32
                             , msgEndpoint :: Text
                             , msgData :: RawData }
             | MsgRawSubscribeCmd { msgId :: Word32
                                  , msgDeviceIndex :: Word32
                                  , msgEndpoint :: Text }
             | MsgRawUnsubscribeCmd { msgId :: Word32
                                    , msgDeviceIndex :: Word32
                                    , msgEndpoint :: Text }
               -- generic device messages
             | MsgStopDeviceCmd { msgId :: Word32
                                , msgDeviceIndex :: Word32
                                }
             | MsgStopAllDevices { msgId :: Word32 }
             | MsgVibrateCmd { msgId :: Word32
                             , msgDeviceIndex :: Word32
                             , msgSpeeds :: [ Vibrate ]
                             }
             | MsgLinearCmd { msgId :: Word32
                            , msgDeviceIndex :: Word32
                            , msgVectors :: [ LinearActuate ]
                            }
             | MsgRotateCmd { msgId :: Word32
                            , msgDeviceIndex :: Word32
                            , msgRotations :: [ Rotate ]
                            }
               -- generic sensor messages
             | MsgBatteryLevelCmd { msgId :: Word32
                                  , msgDeviceIndex :: Word32
                                  }
             | MsgBatteryLevelReading { msgId :: Word32
                                      , msgDeviceIndex :: Word32
                                      , msgBatteryLevel :: Double
                                      }
             | MsgRSSILevelCmd { msgId :: Word32
                               , msgDeviceIndex :: Word32
                               }
             | MsgRSSILevelReading { msgId :: Word32
                                   , msgDeviceIndex :: Word32
                                   , msgRSSILevel :: Int
                                   }
  deriving (Show, Eq)


instance ToJSON Message where
  toJSON (MsgOk {..}) = object
    [ "Ok" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgError {..}) = object
    [ "Error" .= object
      [ "Id" .= msgId
      , "ErrorMessage" .= msgErrorMessage
      , "ErrorCode" .= msgErrorCode
      ]
    ]
  toJSON (MsgPing {..}) = object
    [ "Ping" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgRequestServerInfo {..}) = object
    [ "RequestServerInfo" .= object
      [ "Id" .= msgId
      , "ClientName" .= msgClientName
      , "MessageVersion" .= msgMessageVersion
      ]
    ]
  toJSON (MsgServerInfo {..}) = object
    [ "ServerInfo" .= object
      [ "Id" .= msgId
      , "ServerName" .= msgServerName
      , "MessageVersion" .= msgMessageVersion
      , "MaxPingTime" .= msgMaxPingTime
      ]
    ]
  toJSON (MsgStartScanning {..}) = object
    [ "StartScanning" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgStopScanning {..}) = object
    [ "StopScanning" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgScanningFinished {..}) = object
    [ "ScanningFinished" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgRequestDeviceList {..}) = object
    [ "RequestDeviceList" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgDeviceList {..}) = object
    [ "DeviceList" .= object
      [ "Id" .= msgId
      , "Devices" .= msgDevices
      ]
    ]
  toJSON (MsgDeviceAdded {..}) = object
    [ "DeviceAdded" .= object
      [ "Id" .= msgId
      , "DeviceName" .= msgDeviceName
      , "DeviceIndex" .= msgDeviceIndex
      , "DeviceMessages" .= msgDeviceMessages
      ]
    ]
  toJSON (MsgDeviceRemoved {..}) = object
    [ "DeviceRemoved" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      ]
    ]
  toJSON (MsgRawWriteCmd {..}) = object
    [ "RawWriteCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Endpoint" .= msgEndpoint
      , "Data" .= msgData
      , "WriteWithResponse" .= msgWriteWithResponse
      ]
    ]
  toJSON (MsgRawReadCmd {..}) = object
    [ "RawReadCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Endpoint" .= msgEndpoint
      , "ExpectedLength" .= msgExpectedLength
      , "WaitForData" .= msgWaitForData
      ]
    ]
  toJSON (MsgRawReading {..}) = object
    [ "RawReading" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Endpoint" .= msgEndpoint
      , "Data" .= msgData
      ]
    ]
  toJSON (MsgRawSubscribeCmd {..}) = object
    [ "RawSubscribeCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Endpoint" .= msgEndpoint
      ]
    ]
  toJSON (MsgRawUnsubscribeCmd {..}) = object
    [ "RawUnsubscribeCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Endpoint" .= msgEndpoint
      ]
    ]
  toJSON (MsgStopDeviceCmd {..}) = object
    [ "StopDeviceCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      ]
    ]
  toJSON (MsgStopAllDevices {..}) = object
    [ "StopAllDevices" .= object
      [ "Id" .= msgId
      ]
    ]
  toJSON (MsgVibrateCmd {..}) = object
    [ "VibrateCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Speeds" .= msgSpeeds
      ]
    ]
  toJSON (MsgLinearCmd {..}) = object
    [ "LinearCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Vectors" .= msgVectors
      ]
    ]
  toJSON (MsgRotateCmd {..}) = object
    [ "RotateCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "Rotations" .= msgRotations
      ]
    ]
  toJSON (MsgBatteryLevelCmd {..}) = object
    [ "BatteryLevelCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      ]
    ]
  toJSON (MsgBatteryLevelReading {..}) = object
    [ "BatteryLevelReading" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "BatteryLevel" .= msgBatteryLevel
      ]
    ]
  toJSON (MsgRSSILevelCmd {..}) = object
    [ "RSSILevelCmd" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      ]
    ]
  toJSON (MsgRSSILevelReading {..}) = object
    [ "RSSILevelReading" .= object
      [ "Id" .= msgId
      , "DeviceIndex" .= msgDeviceIndex
      , "RSSILevel" .= msgRSSILevel
      ]
    ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> asum
    [ MsgOk <$> (o .: "Ok" >>= (.: "Id")),
      MsgError
        <$> (o .: "Error" >>= (.: "Id"))
        <*> (o .: "Error" >>= (.: "ErrorMessage"))
        <*> (o .: "Error" >>= (.: "ErrorCode")),
      MsgPing <$> (o .: "Ping" >>= (.: "Id")),
      MsgRequestServerInfo
        <$> (o .: "RequestServerInfo" >>= (.: "Id"))
        <*> (o .: "RequestServerInfo" >>= (.: "ClientName"))
        <*> (o .: "RequestServerInfo" >>= (.: "MessageVersion")),
      MsgServerInfo
        <$> (o .: "ServerInfo" >>= (.: "Id"))
        <*> (o .: "ServerInfo" >>= (.: "ServerName"))
        <*> (o .: "ServerInfo" >>= (.: "MessageVersion"))
        <*> (o .: "ServerInfo" >>= (.: "MaxPingTime")),
      MsgStartScanning  <$> (o .: "StartScanning" >>= (.: "Id")),
      MsgStopScanning <$> (o .: "StopScanning" >>= (.: "Id")),
      MsgScanningFinished  <$> (o .: "ScanningFinished" >>= (.: "Id")),
      MsgRequestDeviceList <$> (o .: "RequestDeviceList" >>= (.: "Id")),
      MsgDeviceList
        <$> (o .: "DeviceList" >>= (.: "Id"))
        <*> (o .: "DeviceList" >>= (.: "Devices")),
      MsgDeviceAdded
        <$> (o .: "DeviceAdded" >>= (.: "Id"))
        <*> (o .: "DeviceAdded" >>= (.: "DeviceName"))
        <*> (o .: "DeviceAdded" >>= (.: "DeviceIndex"))
        <*> (o .: "DeviceAdded" >>= (.: "DeviceMessages")),
      MsgDeviceRemoved
        <$> (o .: "DeviceRemoved" >>= (.: "Id"))
        <*> (o .: "DeviceRemoved" >>= (.: "DeviceIndex")),
      MsgRawWriteCmd
        <$> (o .: "RawWriteCmd" >>= (.: "Id"))
        <*> (o .: "RawWriteCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "RawWriteCmd" >>= (.: "Endpoint"))
        <*> (o .: "RawWriteCmd" >>= (.: "Data"))
        <*> (o .: "RawWriteCmd" >>= (.: "WriteWithResponse")),
      MsgRawReadCmd
        <$> (o .: "RawReadCmd" >>= (.: "Id"))
        <*> (o .: "RawReadCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "RawReadCmd" >>= (.: "Endpoint"))
        <*> (o .: "RawReadCmd" >>= (.: "ExpectedLength"))
        <*> (o .: "RawReadCmd" >>= (.: "WaitForData")),
      MsgRawReading
        <$> (o .: "RawReading" >>= (.: "Id"))
        <*> (o .: "RawReading" >>= (.: "DeviceIndex"))
        <*> (o .: "RawReading" >>= (.: "Endpoint"))
        <*> (o .: "RawReading" >>= (.: "Data")),
      MsgRawSubscribeCmd
        <$> (o .: "RawSubscribeCmd" >>= (.: "Id"))
        <*> (o .: "RawSubscribeCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "RawSubscribeCmd" >>= (.: "Endpoint")),
      MsgRawUnsubscribeCmd
        <$> (o .: "RawUnsubscribeCmd" >>= (.: "Id"))
        <*> (o .: "RawUnsubscribeCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "RawUnsubscribeCmd" >>= (.: "Endpoint")),
      MsgStopDeviceCmd
        <$> (o .: "StopDeviceCmd" >>= (.: "Id"))
        <*> (o .: "StopDeviceCmd" >>= (.: "DeviceIndex")),
      MsgStopAllDevices <$> (o .: "StopAllDevices" >>= (.: "Id")),
      MsgVibrateCmd
        <$> (o .: "VibrateCmd" >>= (.: "Id"))
        <*> (o .: "VibrateCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "VibrateCmd" >>= (.: "Speeds")),
      MsgLinearCmd
        <$> (o .: "LinearCmd" >>= (.: "Id"))
        <*> (o .: "LinearCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "LinearCmd" >>= (.: "Vectors")),
      MsgRotateCmd
        <$> (o .: "RotateCmd" >>= (.: "Id"))
        <*> (o .: "RotateCmd" >>= (.: "DeviceIndex"))
        <*> (o .: "RotateCmd" >>= (.: "Rotations")),
      MsgBatteryLevelCmd
        <$> (o .: "BatteryLevelCmd" >>= (.: "Id"))
        <*> (o .: "BatteryLevelCmd" >>= (.: "DeviceIndex")),
      MsgBatteryLevelReading
        <$> (o .: "BatteryLevelReading" >>= (.: "Id"))
        <*> (o .: "BatteryLevelReading" >>= (.: "DeviceIndex"))
        <*> (o .: "BatteryLevelReading" >>= (.: "BatteryLevel")),
      MsgRSSILevelCmd
        <$> (o .: "RSSILevelCmd" >>= (.: "Id"))
        <*> (o .: "RSSILevelCmd" >>= (.: "DeviceIndex")),
      MsgRSSILevelReading
        <$> (o .: "RSSILevelReading" >>= (.: "Id"))
        <*> (o .: "RSSILevelReading" >>= (.: "DeviceIndex"))
        <*> (o .: "RSSILevelReading" >>= (.: "RSSILevel"))
    ]
