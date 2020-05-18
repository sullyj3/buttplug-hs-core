{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Buttplug.Devices where

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

data MessageAttributes = MessageAttributes 
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

data DeviceMessageType =
  -- Generic commands
    SingleMotorVibrateCmd -- deprecated
  | VibrateCmd
  | LinearCmd
  | RotateCmd
  | RawWriteCmd
  | RawReadCmd
  | StopDeviceCmd
  | SubscribeCmd
  | UnsubscribeCmd

  -- Deprecated device specific commands
  | FleshlightLaunchFW12Cmd
  | LovenseCmd
  | KiirooCmd
  | VorzeA10CycloneCmd
  deriving (Generic, Show, Eq, Ord)

instance ToJSON DeviceMessageType where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON DeviceMessageType where
  parseJSON = genericParseJSON pascalCaseOptions

instance ToJSONKey DeviceMessageType where
  toJSONKey = genericToJSONKey pascalCaseKeyOptions

instance FromJSONKey DeviceMessageType where
  fromJSONKey = genericFromJSONKey pascalCaseKeyOptions
