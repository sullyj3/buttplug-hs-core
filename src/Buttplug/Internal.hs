{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buttplug.Internal where

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

import           Data.Text.Encoding           ( decodeUtf8 )
import           GHC.Generics
import           Data.Text                    ( Text )
import qualified Data.Text                    as T
import           Data.ByteString              ( ByteString )
import qualified Data.ByteString              as BS
import qualified Data.Word                    ( Word8 )
import qualified Network.WebSockets           as WS
import           Data.Aeson                   ( ToJSON(..)
                                              , FromJSON(..)
                                              , genericToJSON
                                              , Options(..)
                                              , defaultOptions
                                              , SumEncoding(..)
                                              , genericParseJSON )
import qualified Data.Map.Strict              as Map
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
clientMessageVersion = 1

stripPrefix :: String -> String -> String
stripPrefix s = drop $ length s

------------------------------------------------
newtype RawCommand = RawCommand ByteString
  deriving (Generic, Show, Eq)


instance ToJSON RawCommand where
  toJSON (RawCommand bs) = toJSON $ BS.unpack bs


instance FromJSON RawCommand where
  parseJSON j = RawCommand . BS.pack <$> parseJSON j


------------------------------------------------
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


------------------------------------------------
data LogLevel = LogLevelOff
              | LogLevelFatal
              | LogLevelError
              | LogLevelWarn
              | LogLevelInfo
              | LogLevelDebug
              | LogLevelTrace
  deriving (Generic, Show, Eq)


instance ToJSON LogLevel where
  toJSON = genericToJSON $ defaultOptions { constructorTagModifier = stripPrefix "LogLevel" }


instance FromJSON LogLevel where
  parseJSON = genericParseJSON $ 
    defaultOptions { constructorTagModifier = stripPrefix "LogLevel" }


------------------------------------------------
data Message = 
               -- handshake messages
               RequestServerInfo { msgId :: Int
                                 , msgClientName :: Text
                                 , msgMessageVersion :: Int
                                 }
             | ServerInfo { msgId :: Int
                          , msgServerName :: Text
                          , msgMajorVersion :: Int
                          , msgMinorVersion :: Int
                          , msgBuildVersion :: Int
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
             | Test { msgId :: Int
                    , msgTestString :: Text }
             | RequestLog { msgId :: Int
                          , msgLogLevel :: LogLevel }
             | Log { msgId :: Int
                   , msgLogLevel :: LogLevel
                   , msgLogMessage :: Text
                   }
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
               -- generic device messages
             | StopDeviceCmd { msgId :: Int
                             , msgDeviceIndex :: Int
                             }
             | StopAllDevices { msgId :: Int }
             -- TODO RawCmd ... how to handle byte array? Spec says it's a JSON array of ints 0-255
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
             -- Specific device messages
             | KiirooCmd { msgId :: Int
                         , msgDeviceIndex :: Int
                         , msgCommand :: Text
                         }
             | FleshlightLaunchFW12Cmd
                 { msgId :: Int
                 , msgDeviceIndex :: Int
                 , msgPosition :: Int
                 , msgSpeed :: Int
                 }
             | LovenseCmd
                 { msgId :: Int
                 , msgDeviceIndex :: Int
                 , msgCommand :: Text
                 }
             | VorzeA10CycloneCmd
                 { msgId :: Int
                 , msgDeviceIndex :: Int
                 , msgSpeed :: Int
                 , msgClockwise :: Bool
                 }
  deriving (Show, Eq, Generic)


instance ToJSON Message where
  toJSON = genericToJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                             , fieldLabelModifier = stripPrefix "msg" }


instance FromJSON Message where
  parseJSON = genericParseJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField
                                                   , fieldLabelModifier = stripPrefix "msg" }


--------------------------------------------------------------------------------







data ButtPlugConnection = WebSocketConnection { con  :: WS.Connection
                                              }


data Connector = WebSocketConnector { wsConnectorHost :: String
                                    , wsConnectorPort :: Int 
                                    }

-- messages that we may expect as a response to other messages
isServerInfo :: Message -> Bool
isServerInfo = \case
  (ServerInfo {}) -> True
  _               -> False

isOk  :: Message -> Bool
isOk = \case
  (Ok {}) -> True
  _       -> False

isTest :: Message -> Bool
isTest = \case
  (Test {}) -> True
  _         -> False

isScanningFinished :: Message -> Bool
isScanningFinished = \case
  (ScanningFinished {}) -> True
  _                     -> False

isDeviceList :: Message -> Bool
isDeviceList = \case
  (DeviceList {}) -> True
  _              -> False


