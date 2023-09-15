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

import           GHC.Generics
import           Control.Monad       (foldM)
import           Data.Map.Strict     (Map)
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import           Data.Aeson.Types    ( Parser )
import           Data.Aeson          ( ToJSON(..)
                                     , FromJSON(..)
                                     , ToJSONKey(..)
                                     , FromJSONKey(..)
                                     , (.=)
                                     , (.:?)
                                     , Value(..)
                                     , object
                                     , genericToJSON
                                     , genericToJSONKey
                                     , genericFromJSONKey
                                     , genericParseJSON
                                     , withObject )
import           Data.Word           ( Word32 )
import qualified Data.HashMap.Strict as HMap

import Buttplug.Core.Internal.JSONUtils

-- | For a particular actuation feature (Vibration, Rotation, or Linear), 
-- represents how many of that feature the device has, and the available 
-- resolution of control of that feature. See
-- (<https://buttplug-spec.docs.buttplug.io/enumeration.html#message-attributes-for-devicelist-and-deviceadded>)
-- for details.
data MessageAttributes = MessageAttributes
       { attrFeatureCount :: Maybe Word32
       , attrStepCount :: Maybe [Word32] }
  deriving (Generic, Show, Eq)


instance ToJSON MessageAttributes where
  toJSON = genericToJSON msgAttributeOptions

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
  deriving (Generic, Show, Eq)

instance ToJSON Device where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON Device where
  parseJSON = genericParseJSON pascalCaseOptions
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
  deriving (Generic, Show, Eq, Ord)

instance ToJSON DeviceMessageType where
  toJSON = genericToJSON deviceMessageOptions

instance FromJSON DeviceMessageType where
  parseJSON = genericParseJSON deviceMessageOptions

instance ToJSONKey DeviceMessageType where
  toJSONKey = genericToJSONKey (stripPrefixKeyOptions "Dev")

instance FromJSONKey DeviceMessageType where
  fromJSONKey = genericFromJSONKey (stripPrefixKeyOptions "Dev")
