{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
module Buttplug.Device where

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
import qualified Data.HashMap.Strict as HMap

import Buttplug.Internal.JSONUtils

data MessageAttributes = MessageAttributes
       { attrFeatureCount :: Maybe Word
       , attrStepCount :: Maybe [Word] }
  deriving (Generic, Show, Eq)


-- TODO test these
instance ToJSON MessageAttributes where
  toJSON = genericToJSON msgAttributeOptions

instance FromJSON MessageAttributes where
  parseJSON = withObject "MessageAttributes" \v -> MessageAttributes
    <$> v .:? "FeatureCount"
    <*> v .:? "StepCount"


---------------------------------------------------------------
data Device =
       Device { deviceName :: Text
              , deviceIndex :: Word
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
