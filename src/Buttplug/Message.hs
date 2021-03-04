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

module Buttplug.Message where

import           GHC.Generics
import           Data.Text                    ( Text )
import           Data.ByteString              ( ByteString )
import qualified Data.ByteString              as BS
import           Data.Aeson                   ( ToJSON(..)
                                              , FromJSON(..)
                                              , genericToJSON
                                              , Options(..)
                                              , SumEncoding(..)
                                              , genericParseJSON 
                                              )
import           Data.Map.Strict              ( Map )

import qualified Buttplug.Device              as Dev
import           Buttplug.Device              ( Device(..) )
import           Buttplug.Internal.JSONUtils


-- | The version of the Buttplug message protocol that the client speaks.
-- (currently version 2)
clientMessageVersion :: Word
clientMessageVersion = 2
------------------------------------------------


-- | Errors from the server, used in the Error message.
-- https://buttplug-spec.docs.buttplug.io/status.html#error
data ErrorCode = ERROR_UNKNOWN  -- ^ An unknown error occurred. 
               | ERROR_INIT     -- ^ Handshake did not succeed.
               | ERROR_PING     -- ^ A ping was not sent in the expected time.
               | ERROR_MSG      -- ^ A message parsing or permission error occurred.
               | ERROR_DEVICE   -- ^ A command sent to a device returned an error.
               deriving (Enum, Show, Eq, Generic)


errCodeFromInt :: Int -> Maybe ErrorCode
errCodeFromInt = \case
  0 -> Just ERROR_UNKNOWN
  1 -> Just ERROR_INIT
  2 -> Just ERROR_PING
  3 -> Just ERROR_MSG
  4 -> Just ERROR_DEVICE
  _ -> Nothing


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

------------------------------------------------
-- used for the various Raw* commands. Circumvents the fact that Aeson doesn't 
-- have bytestring encoding/decoding in genericToJSON and genericParseJSON
newtype RawData = RawData ByteString
  deriving (Generic, Show, Eq)


instance ToJSON RawData where
  toJSON (RawData bs) = toJSON $ BS.unpack bs


instance FromJSON RawData where
  parseJSON j = RawData . BS.pack <$> parseJSON j

------------------------------------------------
-- Used in VibrateCmd to specify the speed of the motor at the given index
data Vibrate = Vibrate { vibrateIndex :: Word
                       , vibrateSpeed :: Double
                       }
  deriving (Generic, Show, Eq)


instance ToJSON Vibrate where
  toJSON = genericToJSON (stripPrefixOptions "vibrate")


instance FromJSON Vibrate where
  parseJSON = genericParseJSON (stripPrefixOptions "vibrate")


------------------------------------------------
data Rotate = Rotate
  { rotateIndex :: Word
  , rotateSpeed :: Double
  , rotateClockwise :: Bool
  }
  deriving (Generic, Show, Eq)

instance ToJSON Rotate where
  toJSON = genericToJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "rotate" }


instance FromJSON Rotate where
  parseJSON = genericParseJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "rotate" }


------------------------------------------------
data LinearActuate = LinearActuate
  { linActIndex :: Word
  , linActDuration :: Word
  , linActPosition :: Double
  }
  deriving (Generic, Show, Eq)


instance ToJSON LinearActuate where
  toJSON = genericToJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "linAct" }


instance FromJSON LinearActuate where
  parseJSON = genericParseJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "linAct" }


-- TODO technically Ids should be Word32, since the maximum id is 4294967295.
-- Not sure whether this applies to other unsigned fields, should find out
------------------------------------------------
data Message =
               -- status messages
               Ok { msgId :: Word }
             | Error { msgId :: Word
                     , msgErrorMessage :: Text
                     , msgErrorCode :: ErrorCode
                     }
             | Ping { msgId :: Word }
               -- handshake messages
             | RequestServerInfo { msgId :: Word
                                 , msgClientName :: Text
                                 , msgMessageVersion :: Word
                                 }
             | ServerInfo { msgId :: Word
                          , msgServerName :: Text
                          , msgMessageVersion :: Word
                          , msgMaxPingTime :: Word
                          }
               -- enumeration messages
             | StartScanning { msgId :: Word }
             | StopScanning { msgId :: Word }
             | ScanningFinished { msgId :: Word }
             | RequestDeviceList { msgId :: Word }
             | DeviceList { msgId :: Word
                          , msgDevices :: [ Device ]
                          }
             | DeviceAdded { msgId :: Word
                           , msgDeviceName :: Text
                           , msgDeviceIndex :: Word
                           , msgDeviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
                           }
             | DeviceRemoved { msgId :: Word
                             , msgDeviceIndex :: Word
                             }
               -- raw device messages
             | RawWriteCmd { msgId :: Word
                           , msgDeviceIndex :: Word
                           , msgEndpoint :: Text
                           , msgData :: RawData
                           , msgWriteWithResponse :: Bool }
             | RawReadCmd { msgId :: Word
                          , msgDeviceIndex :: Word
                          , msgEndpoint :: Text
                          , msgExpectedLength :: Word
                          , msgWaitForData :: Bool }
             | RawReading { msgId :: Word
                          , msgDeviceIndex :: Word
                          , msgEndpoint :: Text
                          , msgData :: RawData }
             | RawSubscribeCmd { msgId :: Word
                               , msgDeviceIndex :: Word
                               , msgEndpoint :: Text }
             | RawUnsubscribeCmd { msgId :: Word
                                 , msgDeviceIndex :: Word
                                 , msgEndpoint :: Text }
               -- generic device messages
             | StopDeviceCmd { msgId :: Word
                             , msgDeviceIndex :: Word
                             }
             | StopAllDevices { msgId :: Word }
             | VibrateCmd { msgId :: Word
                          , msgDeviceIndex :: Word
                          , msgSpeeds :: [ Vibrate ]
                          }
             | LinearCmd { msgId :: Word
                         , msgDeviceIndex :: Word
                         , msgVectors :: [ LinearActuate ]
                         }
             | RotateCmd { msgId :: Word
                         , msgDeviceIndex :: Word
                         , msgRotations :: [ Rotate ]
                         }
               -- generic sensor messages
             | BatteryLevelCmd { msgId :: Word
                               , msgDeviceIndex :: Word
                               }
             | BatteryLevelReading { msgId :: Word
                                   , msgDeviceIndex :: Word
                                   , msgBatteryLevel :: Double
                                   }
             | RSSILevelCmd { msgId :: Word
                            , msgDeviceIndex :: Word
                            }
             | RSSILevelReading { msgId :: Word
                                , msgDeviceIndex :: Word
                                , msgRSSILevel :: Int
                                }
  deriving (Show, Eq, Generic)


instance ToJSON Message where
  toJSON = genericToJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                             , fieldLabelModifier = stripPrefix "msg" }


instance FromJSON Message where
  parseJSON = genericParseJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                                   , fieldLabelModifier = stripPrefix "msg" }
