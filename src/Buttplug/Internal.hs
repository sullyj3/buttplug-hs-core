{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Concurrent.Async
import           Data.Void
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Data.Text.Encoding  (decodeUtf8)
import           Network.Socket      (withSocketsDo)
import           GHC.Generics
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import qualified Data.Word           (Word8)
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
close = do
  (WebSocketConnection con) <- getConnection
  liftIO $ WS.sendClose con ("Bye!" :: Text)


getConnection :: ButtPlugM ButtPlugConnection
getConnection = ask


stopDevice :: Device -> ButtPlugM ()
stopDevice dev@(Device {deviceName=dName, deviceIndex=dIdx})
  -- temporary hack until server handles StopDeviceCmd for youou correctly
  | "Youou" `T.isInfixOf` dName = stopYouou dev
  | otherwise = do
      let msg = StopDeviceCmd { msgId = 1, msgDeviceIndex = dIdx }
      sendMessage msg
  where
    stopYouou :: Device -> ButtPlugM ()
    stopYouou dev = vibrateOnlyMotor dev $ 1/255


vibrateOnlyMotor :: Device -> Double -> ButtPlugM ()
vibrateOnlyMotor (Device {deviceIndex = dIdx}) speed = do
  let msg = VibrateCmd { msgId = 1
                       , msgDeviceIndex = dIdx
                       , msgSpeeds = [MotorVibrate { index = 0
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
  withSocketsDo $ WS.runClient host port "/" \wsCon ->
    let con = WebSocketConnection wsCon
    in
    withWorker (handleReceivedData wsCon) $ runButtPlugM con do
      liftIO $ putStrLn "Connected!"
      sendMessages [reqServerInfo, reqDeviceList, startScanning]
      liftIO $ T.putStrLn "Press enter to exit"
      liftIO getLine
      close
  where
    reqServerInfo = RequestServerInfo
                      { msgId = 1
                      , msgClientName = "haskell"
                      , msgMessageVersion = clientMessageVersion }
    reqDeviceList = RequestDeviceList 1
    startScanning = StartScanning 1

    handleReceivedData :: WS.Connection -> IO Void
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


runClient :: String
          -> Int
          -> ButtPlugM ()
          -> IO ()
runClient host port bpApp = withSocketsDo $ WS.runClient host port "/" \wsCon -> do
  putStrLn "Connected!"
  runButtPlugM (WebSocketConnection wsCon) bpApp


-- From https://mazzo.li/posts/threads-resources.html
withWorker ::
     IO Void -- ^ Worker to run
  -> IO a
  -> IO a
withWorker worker cont = either absurd Prelude.id <$> race worker cont
