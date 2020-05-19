{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
                , ButtPlugM
                , ButtPlugApp(..)
                , runButtPlugM
                , runButtPlugWSApp
                , getConnection
                , vibrateOnlyMotor
                )-} 

import           Data.Foldable       (traverse_)
import           Control.Monad       (forever)
import           Control.Concurrent (forkIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Data.Text.Encoding  (decodeUtf8)
import           Network.Socket      (withSocketsDo)
import           GHC.Generics
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import           Data.Aeson          ( ToJSON(..)
                                     , FromJSON(..)
                                     , genericToJSON
                                     , Options(..)
                                     , defaultOptions
                                     , SumEncoding(..)
                                     , genericParseJSON
                                     , encode
                                     , decode)
-- import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
import           Data.ByteString.Lazy (fromStrict)
import           UnliftIO.Exception

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device(..))
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
  { index :: Int
  , duration :: Int
  , clockwise :: Bool
  }
  deriving (Generic, Show, Eq)

instance ToJSON Rotate where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON Rotate where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data LinearActuate = LinearActuate
  { index :: Int
  , duration :: Int
  , position :: Double
  }
  deriving (Generic, Show, Eq)

instance ToJSON LinearActuate where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON LinearActuate where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data LogLevel = LogLevelOff
              | LogLevelFatal
              | LogLevelError
              | LogLevelWarn
              | LogLevelInfo
              | LogLevelDebug
              | LogLevelTrace
  deriving (Generic, Show, Eq)

stripLogLevel = drop $ length $ ("LogLevel" :: String)

instance ToJSON LogLevel where
  toJSON = genericToJSON $ defaultOptions { constructorTagModifier = stripLogLevel }

instance FromJSON LogLevel where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = stripLogLevel }
------------------------------------------------
data Message = 
               -- handshake messages
               RequestServerInfo { id :: Int
                                 , clientName :: Text
                                 , messageVersion :: Int
                                 }
             | ServerInfo { id :: Int
                          , serverName :: Text
                          , majorVersion :: Int
                          , minorVersion :: Int
                          , buildVersion :: Int
                          , messageVersion :: Int
                          , maxPingTime :: Int
                          }
               -- status messages
             | Ok { id :: Int }
             | Error { id :: Int
                     , errorMessage :: Text
                     , errorCode :: ErrorCode
                     }
             | Ping { id :: Int }
             | Test { id :: Int
                    , testString :: Text }
             | RequestLog { id :: Int
                          , logLevel :: LogLevel }
             | Log { id :: Int
                   , logLevel :: LogLevel
                   , logMessage :: Text
                   }
               -- enumeration messages
             | StartScanning { id :: Int }
             | StopScanning { id :: Int }
             | ScanningFinished { id :: Int }
             | RequestDeviceList { id :: Int }
             | DeviceList { id :: Int
                          , devices :: [ Device ]
                          }
             | DeviceAdded { id :: Int
                           , deviceName :: Text
                           , deviceIndex :: Int
                           , deviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
                           }
             | DeviceRemoved { id :: Int
                             , deviceIndex :: Int
                             }
               -- generic device messages
             | StopDeviceCmd { id :: Int
                             , deviceIndex :: Int
                             }
             | StopAllDevices { id :: Int }
             -- TODO RawCmd ... how to handle byte array? Spec says it's a JSON array of ints 0-255
             | VibrateCmd { id :: Int
                          , deviceIndex :: Int
                          , speeds :: [ MotorVibrate ]
                          }
             | LinearCmd { id :: Int
                         , deviceIndex :: Int
                         , vectors :: [ LinearActuate ]
                         }
             | RotateCmd { id :: Int
                         , deviceIndex :: Int
                         , rotations :: [ Rotate ]
                         }
             -- Specific device messages
             | KiirooCmd { id :: Int
                         , deviceIndex :: Int
                         , command :: Text
                         }
             | FleshlightLaunchFW12Cmd
                 { id :: Int
                 , deviceIndex :: Int
                 , position :: Int
                 , speed :: Int
                 }
             | LovenseCmd
                 { id :: Int
                 , deviceIndex :: Int
                 , command :: Text
                 }
             | VorzeA10CycloneCmd
                 { id :: Int
                 , deviceIndex :: Int
                 , speed :: Int
                 , clockwise :: Bool
                 }
  deriving (Show, Eq, Generic)




instance ToJSON Message where
  toJSON = genericToJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField }

instance FromJSON Message where
  parseJSON = genericParseJSON $ pascalCaseOptions { sumEncoding = ObjectWithSingleField }

--------------------------------------------------------------------------------

receiveMsgs :: WS.Connection -> IO [Message]
receiveMsgs con = do
  received <- WS.receiveData con
  T.putStrLn $ "Server sent: " <> decodeUtf8 received
  case decode $ fromStrict received :: Maybe [Message] of
    Just msgs -> pure msgs
    Nothing -> throwString "Couldn't decode the message from the server"
  
sendMessage :: Message -> ButtPlugM ()
sendMessage msg = sendMessages [msg]

sendMessages :: [Message] -> ButtPlugM ()
sendMessages  msgs = do
  (WebSocketConnection con) <- getConnection
  liftIO $ WS.sendTextData con (encode msgs)

close :: ButtPlugM ()
close  = do
  (WebSocketConnection con) <- getConnection
  liftIO $ WS.sendClose con ("Bye!" :: Text)

getConnection :: ButtPlugM ButtPlugConnection
getConnection = ask


stopDevice :: Device -> ButtPlugM ()
stopDevice dev@(Device {deviceName=dName, deviceIndex=dIdx})
  -- temporary hack until server handles StopDeviceCmd for youou correctly
  | "Youou" `T.isInfixOf` dName = stopYouou dev
  | otherwise = do
      let msg = StopDeviceCmd { id = 1, deviceIndex = dIdx }
      sendMessage msg
  where
    stopYouou :: Device -> ButtPlugM ()
    stopYouou dev = vibrateOnlyMotor dev $ 1/255

vibrateOnlyMotor :: Device -> Double -> ButtPlugM ()
vibrateOnlyMotor (Device {deviceIndex = dIdx}) speed = do
  let msg = VibrateCmd { id = 1
                       , deviceIndex = dIdx
                       , speeds = [MotorVibrate { index = 0
                                                , speed = speed }]
                       }
  con <- getConnection
  sendMessage msg

data ButtPlugConnection = WebSocketConnection { con  :: WS.Connection
                                              }

data Connector = WebSocketConnector { wsConnectorHost :: String
                                    , wsConnectorPort :: Int 
                                    }

type ButtPlugM a = ReaderT ButtPlugConnection IO a 

data ButtPlugApp = ButtPlugApp 
  { handleDeviceAdded :: Device -> ButtPlugM ()
  }

runButtPlugM :: ButtPlugConnection -> ButtPlugM a -> IO a
runButtPlugM con bpm = runReaderT bpm con

runButtPlugApp :: Connector
               -> ButtPlugApp
               -> IO ()
runButtPlugApp (WebSocketConnector host port) (ButtPlugApp handleDeviceAdded) = 
  withSocketsDo $ WS.runClient host port "/" \wsCon -> do
    putStrLn "Connected!"

    let
        con = WebSocketConnection wsCon
        reqServerInfo = RequestServerInfo
                          { id = 1
                          , clientName = "haskell"
                          , messageVersion = clientMessageVersion }
        reqDeviceList = RequestDeviceList 1
        startScanning = StartScanning 1

    -- WS.sendTextData con (encode [reqServerInfo, reqDeviceList, startScanning])
    runButtPlugM con $ sendMessages [reqServerInfo, reqDeviceList, startScanning]

    _ <- forkIO $ handleReceivedData wsCon

    T.putStrLn "Press enter to exit"
    _ <- getLine
    liftIO $ WS.sendClose wsCon ("Bye!" :: Text)

    pure ()
  where

    handleReceivedData :: WS.Connection -> IO ()
    handleReceivedData con = do
      putStrLn "Listening for messages from server"
      forever $ receiveMsgs con >>= handleMsgs con

    handleMsgs :: WS.Connection -> [Message] -> IO ()
    handleMsgs con msgs = do
      putStrLn "Received some messages."
      traverse_ (handleMsg con) msgs

    --handleMsg :: ButtPlugConnection -> Message -> IO ()
    handleMsg :: WS.Connection -> Message -> IO ()
    handleMsg con msg = do
      putStrLn "Handling message:"
      print $ msg
      case msg of
        (DeviceAdded _msgId name idx allowedMsgs) -> do
          let dev = Dev.Device name idx allowedMsgs
          _ <- forkIO $ runButtPlugM (WebSocketConnection con)
                                     (handleDeviceAdded dev)
          pure ()

        msg -> do
          putStrLn "No handler supplied"
      putStrLn "-------------------"

void ma = do
  _ <- ma
  pure ()


runClient :: String
          -> Int
          -> ButtPlugM ()
          -> IO ()
runClient host port bpApp = withSocketsDo $ WS.runClient host port "/" \wsCon -> do
    putStrLn "Connected!"
    app wsCon
  where -- app = bpApp . WebSocketConnection
        app wsCon = runReaderT bpApp (WebSocketConnection wsCon)

-- "{\"DeviceName\":\"Youou Wand Vibrator\",\"DeviceMessages\":{\"SingleMotorVibrateCmd\":{},\"VibrateCmd\":{\"FeatureCount\":1},\"StopDeviceCmd\":{}},\"DeviceIndex\":1,\"Id\":0}}"

--------------------------------------------------------------------------------
