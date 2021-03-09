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

import qualified Buttplug.Core.Device              as Dev
import           Buttplug.Core.Device              ( Device(..) )
import           Buttplug.Core.Internal.JSONUtils


-- | The version of the Buttplug message protocol that the client speaks.
-- (currently version 2)
clientMessageVersion :: Word
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
               deriving (Enum, Show, Eq, Generic)


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
  deriving (Generic, Show, Eq)


instance ToJSON RawData where
  toJSON (RawData bs) = toJSON $ BS.unpack bs


instance FromJSON RawData where
  parseJSON j = RawData . BS.pack <$> parseJSON j

-- | Used in VibrateCmd to specify the speed of the motor at the given index
data Vibrate = Vibrate { vibrateIndex :: Word
                       , vibrateSpeed :: Double
                       }
  deriving (Generic, Show, Eq)


instance ToJSON Vibrate where
  toJSON = genericToJSON (stripPrefixOptions "vibrate")


instance FromJSON Vibrate where
  parseJSON = genericParseJSON (stripPrefixOptions "vibrate")


-- | Used in RotateCmd to specify the speed and direction of rotation of the
-- motor at the given index
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


-- | Used in LinearCmd to specify how to move the linear actuator at the given
-- index
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

-- | The type of Buttplug protocol messages. See
-- (<https://buttplug-spec.docs.buttplug.io/messages.html>) for the protocol
-- specification and an explanation of the purpose of each message.
data Message =
               -- status messages
               MsgOk { msgId :: Word }
             | MsgError { msgId :: Word
                        , msgErrorMessage :: Text
                        , msgErrorCode :: ErrorCode
                        }
             | MsgPing { msgId :: Word }
               -- handshake messages
             | MsgRequestServerInfo { msgId :: Word
                                    , msgClientName :: Text
                                    , msgMessageVersion :: Word
                                    }
             | MsgServerInfo { msgId :: Word
                             , msgServerName :: Text
                             , msgMessageVersion :: Word
                             , msgMaxPingTime :: Word
                             }
               -- enumeration messages
             | MsgStartScanning { msgId :: Word }
             | MsgStopScanning { msgId :: Word }
             | MsgScanningFinished { msgId :: Word }
             | MsgRequestDeviceList { msgId :: Word }
             | MsgDeviceList { msgId :: Word
                             , msgDevices :: [ Device ]
                             }
             | MsgDeviceAdded { msgId :: Word
                              , msgDeviceName :: Text
                              , msgDeviceIndex :: Word
                              , msgDeviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
                              }
             | MsgDeviceRemoved { msgId :: Word
                                , msgDeviceIndex :: Word
                                }
               -- raw device messages
             | MsgRawWriteCmd { msgId :: Word
                              , msgDeviceIndex :: Word
                              , msgEndpoint :: Text
                              , msgData :: RawData
                              , msgWriteWithResponse :: Bool }
             | MsgRawReadCmd { msgId :: Word
                             , msgDeviceIndex :: Word
                             , msgEndpoint :: Text
                             , msgExpectedLength :: Word
                             , msgWaitForData :: Bool }
             | MsgRawReading { msgId :: Word
                             , msgDeviceIndex :: Word
                             , msgEndpoint :: Text
                             , msgData :: RawData }
             | MsgRawSubscribeCmd { msgId :: Word
                                  , msgDeviceIndex :: Word
                                  , msgEndpoint :: Text }
             | MsgRawUnsubscribeCmd { msgId :: Word
                                    , msgDeviceIndex :: Word
                                    , msgEndpoint :: Text }
               -- generic device messages
             | MsgStopDeviceCmd { msgId :: Word
                                , msgDeviceIndex :: Word
                                }
             | MsgStopAllDevices { msgId :: Word }
             | MsgVibrateCmd { msgId :: Word
                             , msgDeviceIndex :: Word
                             , msgSpeeds :: [ Vibrate ]
                             }
             | MsgLinearCmd { msgId :: Word
                            , msgDeviceIndex :: Word
                            , msgVectors :: [ LinearActuate ]
                            }
             | MsgRotateCmd { msgId :: Word
                            , msgDeviceIndex :: Word
                            , msgRotations :: [ Rotate ]
                            }
               -- generic sensor messages
             | MsgBatteryLevelCmd { msgId :: Word
                                  , msgDeviceIndex :: Word
                                  }
             | MsgBatteryLevelReading { msgId :: Word
                                      , msgDeviceIndex :: Word
                                      , msgBatteryLevel :: Double
                                      }
             | MsgRSSILevelCmd { msgId :: Word
                               , msgDeviceIndex :: Word
                               }
             | MsgRSSILevelReading { msgId :: Word
                                   , msgDeviceIndex :: Word
                                   , msgRSSILevel :: Int
                                   }
  deriving (Show, Eq, Generic)


instance ToJSON Message where
  toJSON = genericToJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                             , fieldLabelModifier = stripPrefix "msg"
                                             , constructorTagModifier = stripPrefix "Msg" }


instance FromJSON Message where
  parseJSON = genericParseJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                                   , fieldLabelModifier = stripPrefix "msg"
                                                   , constructorTagModifier = stripPrefix "Msg" }
