{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Buttplug.Core.Device
Description : Types for representing sex toys
Copyright   : (c) James Sully, 2020-2021
License     : BSD 3-Clause
Maintainer  : sullyj3@gmail.com
Stability   : experimental
Portability : untested

Types for representing sex toys, as well as ways of actuating them.
-}

module Buttplug.Core.Device where

import           Data.Map.Strict     (Map)
import           Data.Text           (Text)
import           Data.Aeson.Types    ( toJSONKeyText
                                     , FromJSONKeyFunction(..)
                                     )
import           Data.Aeson          ( ToJSON(..)
                                     , FromJSON(..)
                                     , ToJSONKey(..)
                                     , FromJSONKey(..)
                                     , (.=)
                                     , (.:?)
                                     , (.:)
                                     , Value(..)
                                     , object
                                     , withObject )
import           Data.Word           ( Word32 )

-- | For a particular actuation feature (Vibration, Rotation, or Linear),
-- represents how many of that feature the device has, and the available 
-- resolution of control of that feature. See
-- (<https://buttplug-spec.docs.buttplug.io/enumeration.html#message-attributes-for-devicelist-and-deviceadded>)
-- for details.
data MessageAttributes = MessageAttributes
       { attrFeatureCount :: Maybe Word32
       , attrStepCount :: Maybe [Word32] }
  deriving (Show, Eq)


instance ToJSON MessageAttributes where
  toJSON msgAttr = object
    [ "FeatureCount" .= attrFeatureCount msgAttr,
      "StepCount" .= attrStepCount msgAttr
    ]

instance FromJSON MessageAttributes where
  parseJSON = withObject "MessageAttributes" \v -> MessageAttributes
    <$> v .:? "FeatureCount"
    <*> v .:? "StepCount"

-- | An intimate device, containing info about the functionality it supports.
data Device =
       Device { deviceName :: Text
              , deviceIndex :: Word32
              , deviceMessages :: Map DeviceMessageType MessageAttributes
              }
  deriving (Show, Eq)

instance ToJSON Device where
  toJSON device = object
    [ "DeviceName" .= deviceName device,
      "DeviceIndex" .= deviceIndex device,
      "DeviceMessages" .= deviceMessages device
    ]

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o ->
    Device
      <$> o .: "DeviceName"
      <*> o .: "DeviceIndex"
      <*> o .: "DeviceMessages"

---------------------------------------------------------------

-- | Represents which message types the device supports
-- See
-- (<https://buttplug-spec.docs.buttplug.io/enumeration.html#message-attributes-for-devicelist-and-deviceadded>)
-- for details.
data DeviceMessageType =
  -- Raw Device commands
    DevRawWriteCmd
  | DevRawReadCmd
  | DevRawSubscribeCmd
  | DevRawUnsubscribeCmd
  -- Generic Device commands
  | DevStopDeviceCmd
  | DevVibrateCmd
  | DevLinearCmd
  | DevRotateCmd
  deriving (Show, Eq, Ord)

instance ToJSON DeviceMessageType where
  toJSON DevRawWriteCmd = String "RawWriteCmd"
  toJSON DevRawReadCmd = String "RawReadCmd"
  toJSON DevRawSubscribeCmd = String "RawSubscribeCmd"
  toJSON DevRawUnsubscribeCmd = String "RawUnsubscribeCmd"
  toJSON DevStopDeviceCmd = String "StopDeviceCmd"
  toJSON DevVibrateCmd = String "VibrateCmd"
  toJSON DevLinearCmd = String "LinearCmd"
  toJSON DevRotateCmd = String "RotateCmd"

instance FromJSON DeviceMessageType where
  parseJSON (String "RawWriteCmd") = pure DevRawWriteCmd
  parseJSON (String "RawReadCmd") = pure DevRawReadCmd
  parseJSON (String "RawSubscribeCmd") = pure DevRawSubscribeCmd
  parseJSON (String "RawUnsubscribeCmd") = pure DevRawUnsubscribeCmd
  parseJSON (String "StopDeviceCmd") = pure DevStopDeviceCmd
  parseJSON (String "VibrateCmd") = pure DevVibrateCmd
  parseJSON (String "LinearCmd") = pure DevLinearCmd
  parseJSON (String "RotateCmd") = pure DevRotateCmd
  parseJSON deviceMessageType = fail $ "Cannot decode: " <> (show deviceMessageType)

instance ToJSONKey DeviceMessageType where
  toJSONKey = toJSONKeyText $ \deviceMessageType ->
    case deviceMessageType of
      DevRawWriteCmd -> "RawWriteCmd"
      DevRawReadCmd -> "RawReadCmd"
      DevRawSubscribeCmd -> "RawSubscribeCmd"
      DevRawUnsubscribeCmd -> "RawUnsubscribeCmd"
      DevStopDeviceCmd -> "StopDeviceCmd"
      DevVibrateCmd -> "VibrateCmd"
      DevLinearCmd -> "LinearCmd"
      DevRotateCmd -> "RotateCmd"

instance FromJSONKey DeviceMessageType where
  fromJSONKey = FromJSONKeyTextParser $ \deviceMessageType ->
    case deviceMessageType of
      "RawWriteCmd" -> pure DevRawWriteCmd
      "RawReadCmd" -> pure DevRawReadCmd
      "RawSubscribeCmd" -> pure DevRawSubscribeCmd
      "RawUnsubscribeCmd" -> pure DevRawUnsubscribeCmd
      "StopDeviceCmd" -> pure DevStopDeviceCmd
      "VibrateCmd" -> pure DevVibrateCmd
      "LinearCmd" -> pure DevLinearCmd
      "RotateCmd" -> pure DevRotateCmd
      deviceMessageTypeKey -> fail $ "Cannot decode key: " <> show deviceMessageTypeKey
