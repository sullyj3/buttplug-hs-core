{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
module Buttplug.Device where

import           GHC.Generics
import           Data.Map.Strict     (Map)
import           Data.Text           (Text)
import           Data.Aeson          ( ToJSON(..)
                                     , FromJSON(..)
                                     , ToJSONKey(..)
                                     , FromJSONKey(..)
                                     , (.=)
                                     , Value(..)
                                     , object
                                     , genericToJSON
                                     , genericToJSONKey
                                     , genericFromJSONKey
                                     , genericParseJSON)
import           Data.HashMap.Strict as HMap

import Buttplug.Internal.JSONUtils

newtype MessageAttributes = MessageAttributes 
       { featureCount :: Maybe Int }
  deriving (Generic, Show, Eq)

instance ToJSON MessageAttributes where
  toJSON (MessageAttributes mFeatCount) = case mFeatCount of
    Just n -> object ["FeatureCount" .= n]
    Nothing -> object []

instance FromJSON MessageAttributes where
  parseJSON (Object v) = MessageAttributes <$> case HMap.toList v of
    [("FeatureCount", n)] -> Just <$> parseJSON n
    _                     -> pure Nothing

---------------------------------------------------------------
data Device =
       Device { deviceName :: Text
              , deviceIndex :: Int
              , deviceMessages :: Map DeviceMessageType MessageAttributes
              }
  deriving (Generic, Show, Eq)

instance ToJSON Device where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON Device where
  parseJSON = genericParseJSON pascalCaseOptions
---------------------------------------------------------------

-- Represents which message types the device supports
data DeviceMessageType =
  -- Raw Device commands
    RawWriteCmd
  | RawReadCmd
  | RawSubscribeCmd
  | RawUnsubscribeCmd
  -- Generic Device commands
  | StopDeviceCmd
  | VibrateCmd
  | LinearCmd
  | RotateCmd

  deriving (Generic, Show, Eq, Ord)

instance ToJSON DeviceMessageType where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON DeviceMessageType where
  parseJSON = genericParseJSON pascalCaseOptions

instance ToJSONKey DeviceMessageType where
  toJSONKey = genericToJSONKey pascalCaseKeyOptions

instance FromJSONKey DeviceMessageType where
  fromJSONKey = genericFromJSONKey pascalCaseKeyOptions
