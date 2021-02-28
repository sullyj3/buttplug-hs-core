{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buttplug.Message where

-- TODO split into internal module for testing and user facing module
{-( Message(..)
                , MotorVibrate(..)
                , ButtPlugConnection
                , runClient
                , sendMessage
                , sendMessages
                , close
                , runButtPlugWSApp
                , getConnection
                , vibrateOnlyMotor
                )-} 

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

import qualified Buttplug.Devices             as Dev
import           Buttplug.Devices             ( Device(..) )
import           Buttplug.Internal.JSONUtils

data ErrorCode = ERROR_UNKNOWN
               | ERROR_INIT
               | ERROR_PING
               | ERROR_MSG
               | ERROR_DEVICE
               deriving (Enum, Show, Eq)


errCodeFromInt :: Int -> Maybe ErrorCode
errCodeFromInt = \case
  0 -> Just ERROR_UNKNOWN
  1 -> Just ERROR_INIT
  2 -> Just ERROR_PING
  3 -> Just ERROR_MSG
  4 -> Just ERROR_DEVICE
  _ -> Nothing


fromErrCode :: ErrorCode -> Int
fromErrCode = fromEnum


instance ToJSON ErrorCode where
  toJSON = toJSON . fromErrCode


instance FromJSON ErrorCode where
  parseJSON v = do
    m <- errCodeFromInt <$> parseJSON v
    case m of
      Nothing -> fail "Error code should be an int"
      Just e -> pure e


------------------------------------------------
clientMessageVersion :: Int
clientMessageVersion = 2

stripPrefix :: String -> String -> String
stripPrefix s = drop $ length s


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
data MotorVibrate = MotorVibrate { index :: Int
                                 , speed :: Double
                                 }
  deriving (Generic, Show, Eq)


instance ToJSON MotorVibrate where
  toJSON = genericToJSON pascalCaseOptions


instance FromJSON MotorVibrate where
  parseJSON = genericParseJSON pascalCaseOptions


------------------------------------------------
data Rotate = Rotate
  { rotateIndex :: Int
  , rotateDuration :: Int
  , rotateClockwise :: Bool
  }
  deriving (Generic, Show, Eq)

instance ToJSON Rotate where
  toJSON = genericToJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "rotate" }


instance FromJSON Rotate where
  parseJSON = genericParseJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "rotate" }


------------------------------------------------
data LinearActuate = LinearActuate
  { linActIndex :: Int
  , linActDuration :: Int
  , linActPosition :: Double
  }
  deriving (Generic, Show, Eq)


instance ToJSON LinearActuate where
  toJSON = genericToJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "linAct" }


instance FromJSON LinearActuate where
  parseJSON = genericParseJSON pascalCaseOptions { fieldLabelModifier = stripPrefix "linAct" }



-- TODO: Technically some of these should be unsigned - Word rather than Int
-- deal with it if it comes up
------------------------------------------------
data Message = 
               -- handshake messages
               RequestServerInfo { msgId :: Int
                                 , msgClientName :: Text
                                 , msgMessageVersion :: Int
                                 }
             | ServerInfo { msgId :: Int
                          , msgServerName :: Text
                          , msgMessageVersion :: Int
                          , msgMaxPingTime :: Int
                          }
               -- status messages
             | Ok { msgId :: Int }
             | Error { msgId :: Int
                     , msgErrorMessage :: Text
                     , msgErrorCode :: ErrorCode
                     }
             | Ping { msgId :: Int }
               -- enumeration messages
             | StartScanning { msgId :: Int }
             | StopScanning { msgId :: Int }
             | ScanningFinished { msgId :: Int }
             | RequestDeviceList { msgId :: Int }
             | DeviceList { msgId :: Int
                          , msgDevices :: [ Device ]
                          }
             | DeviceAdded { msgId :: Int
                           , msgDeviceName :: Text
                           , msgDeviceIndex :: Int
                           , msgDeviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
                           }
             | DeviceRemoved { msgId :: Int
                             , msgDeviceIndex :: Int
                             }
               -- raw device messages
             | RawWriteCmd { msgId :: Int
                           , msgDeviceIndex :: Int
                           , msgEndpoint :: Text
                           , msgData :: RawData
                           , msgWriteWithResponse :: Bool }
             | RawReadCmd { msgId :: Int
                          , msgDeviceIndex :: Int
                          , msgEndpoint :: Text
                          , msgExpectedLength :: Int
                          , msgWaitForData :: Bool }
             | RawReading { msgId :: Int
                          , msgDeviceIndex :: Int
                          , msgEndpoint :: Text
                          , msgData :: RawData }
             | RawSubscribeCmd { msgId :: Int
                               , msgDeviceIndex :: Int
                               , msgEndpoint :: Text }
             | RawUnsubscribeCmd { msgId :: Int
                                 , msgDeviceIndex :: Int
                                 , msgEndpoint :: Text }
               -- generic device messages
             | StopDeviceCmd { msgId :: Int
                             , msgDeviceIndex :: Int
                             }
             | StopAllDevices { msgId :: Int }
             | VibrateCmd { msgId :: Int
                          , msgDeviceIndex :: Int
                          , msgSpeeds :: [ MotorVibrate ]
                          }
             | LinearCmd { msgId :: Int
                         , msgDeviceIndex :: Int
                         , msgVectors :: [ LinearActuate ]
                         }
             | RotateCmd { msgId :: Int
                         , msgDeviceIndex :: Int
                         , msgRotations :: [ Rotate ]
                         }
               -- generic sensor messages
             | BatteryLevelCmd { msgId :: Int
                               , msgDeviceIndex :: Int
                               }
             | BatteryLevelReading { msgId :: Int
                               , msgDeviceIndex :: Int
                               , msgBatteryLevel :: Double
                               }
             | RSSILevelCmd { msgId :: Int
                            , msgDeviceIndex :: Int
                            }
             | RSSILevelReading { msgId :: Int
                                , msgDeviceIndex :: Int
                                , msgRSSILevel :: Int
                                }
  deriving (Show, Eq, Generic)


instance ToJSON Message where
  toJSON = genericToJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                             , fieldLabelModifier = stripPrefix "msg" }


instance FromJSON Message where
  parseJSON = genericParseJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                                   , fieldLabelModifier = stripPrefix "msg" }


-- messages that we may expect as a response to other messages
isServerInfo :: Message -> Bool
isServerInfo = \case
  (ServerInfo {}) -> True
  _               -> False

isOk  :: Message -> Bool
isOk = \case
  (Ok {}) -> True
  _       -> False

isScanningFinished :: Message -> Bool
isScanningFinished = \case
  (ScanningFinished {}) -> True
  _                     -> False

isDeviceList :: Message -> Bool
isDeviceList = \case
  (DeviceList {}) -> True
  _              -> False

