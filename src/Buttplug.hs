{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buttplug ( Message(..)
                , DeviceAddedFields(..)
                , RequestServerInfoFields(..)
                , RequestDeviceListFields(..)
                , clientMessageVersion
                , StartScanningFields(..)
                , VibrateCmdFields(..)
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
                ) where

import           Data.Foldable       (traverse_)
import           Control.Monad       (forever)
import           Control.Concurrent (forkIO, threadDelay)
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
                                     , (.=)
                                     , Value(..)
                                     , object
                                     , genericToJSON
                                     , genericParseJSON
                                     , encode
                                     , decode)
-- import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
import           Data.HashMap.Strict as HMap
import           Data.ByteString.Lazy (fromStrict)
import           UnliftIO.Exception

import qualified Buttplug.Devices    as Dev
import           Buttplug.Devices    (Device)
import           Buttplug.JSONUtils


data ErrorCode = ERROR_UNKNOWN
               | ERROR_INIT
               | ERROR_PING
               | ERROR_MSG
               | ERROR_DEVICE
               deriving (Enum, Show)

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
data RequestServerInfoFields = 
  RequestServerInfoFields { id :: Int
                          , clientName :: Text
                          , messageVersion :: Int
                          }
                          deriving (Generic, Show)

instance ToJSON RequestServerInfoFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON RequestServerInfoFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data ServerInfoFields = 
  ServerInfoFields { id :: Int
                   , serverName :: Text
                   , majorVersion :: Int
                   , minorVersion :: Int
                   , buildVersion :: Int
                   , messageVersion :: Int
                   , maxPingTime :: Int
                   }
                   deriving (Generic, Show)

instance ToJSON ServerInfoFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON ServerInfoFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data OkFields = OkFields { id :: Int }
  deriving (Generic, Show)

instance ToJSON OkFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON OkFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data ErrorFields = ErrorFields { id :: Int
                               , errorMessage :: Text
                               , errorCode :: ErrorCode
                               }
  deriving (Generic, Show)

instance ToJSON ErrorFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON ErrorFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data PingFields = PingFields { id :: Int }
  deriving (Generic, Show)

instance ToJSON PingFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON PingFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data StartScanningFields = StartScanningFields { id :: Int }
  deriving (Generic, Show)

instance ToJSON StartScanningFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON StartScanningFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data RequestDeviceListFields = RequestDeviceListFields { id :: Int }
  deriving (Generic, Show)

instance ToJSON RequestDeviceListFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON RequestDeviceListFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data DeviceListFields = 
       DeviceListFields { id :: Int
                        , devices :: [ Device ]
                        }
  deriving (Generic, Show)

instance ToJSON DeviceListFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON DeviceListFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data DeviceAddedFields = DeviceAddedFields 
       { id :: Int
       , deviceName :: Text
       , deviceIndex :: Int
       , deviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
       }
  deriving (Generic, Show)

instance ToJSON DeviceAddedFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON DeviceAddedFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data VibrateCmdFields = VibrateCmdFields { id :: Int
                                         , deviceIndex :: Int
                                         , speeds :: [ MotorVibrate ]
                                         }
  deriving (Generic, Show)

instance ToJSON VibrateCmdFields where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON VibrateCmdFields where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data MotorVibrate = MotorVibrate { index :: Int
                                 , speed :: Double
                                 }
  deriving (Generic, Show)

instance ToJSON MotorVibrate where
  toJSON = genericToJSON pascalCaseOptions

instance FromJSON MotorVibrate where
  parseJSON = genericParseJSON pascalCaseOptions

------------------------------------------------
data Message = 
               -- handshake messages
               RequestServerInfo RequestServerInfoFields
             | ServerInfo ServerInfoFields
               -- status messages
             | Ok OkFields
             | Error ErrorFields
             | Ping PingFields
               -- enumeration messages
             | StartScanning StartScanningFields
             | RequestDeviceList RequestDeviceListFields
             | DeviceList DeviceListFields
             | DeviceAdded DeviceAddedFields
               -- generic device messages
             | VibrateCmd VibrateCmdFields

             -- temporary for debugging purposes
             | UnknownMessage Value
  deriving Show



instance ToJSON Message where
  -- Not yet exhaustive
  toJSON = \case
    (RequestServerInfo fields) ->
      object [ "RequestServerInfo" .= fields ]
    (ServerInfo fields) ->
      object [ "ServerInfo" .= fields ]
    (Ok fields) ->
      object [ "Ok" .= fields ]
    (Error fields) ->
      object [ "Error" .= fields ]
    (Ping fields) ->
      object [ "Ping" .= fields ]
    (StartScanning fields) ->
      object [ "StartScanning" .= fields ]
    (RequestDeviceList fields) ->
      object [ "RequestDeviceList" .= fields ]
    (DeviceList fields) ->
      object [ "DeviceList" .= fields ]
    (DeviceAdded fields) ->
      object [ "DeviceAdded" .= fields ]
    (VibrateCmd fields) ->
      object [ "VibrateCmd" .= fields ]

instance FromJSON Message where
  parseJSON obj@(Object hm) =
    case HMap.toList hm of
      [(msgType, fields)] -> do
        case msgType of
          "RequestServerInfo" -> RequestServerInfo <$> parseJSON fields
          "ServerInfo"        -> ServerInfo        <$> parseJSON fields
          "Ok"                -> Ok                <$> parseJSON fields
          "Error"             -> Error             <$> parseJSON fields
          "Ping"              -> Ping              <$> parseJSON fields
          "StartScanning"     -> StartScanning     <$> parseJSON fields
          "RequestDeviceList" -> RequestDeviceList <$> parseJSON fields
          "DeviceList"        -> DeviceList        <$> parseJSON fields
          "DeviceAdded"       -> DeviceAdded       <$> parseJSON fields
          "VibrateCmd"        -> VibrateCmd        <$> parseJSON fields
          _                   -> pure $ UnknownMessage obj
          -- _ -> fail "Invalid Message type"
      _       -> fail "Invalid Message - There should be only one message type"
  parseJSON _ = fail "Invalid Message - not an object"

--------------------------------------------------------------------------------

receiveMsgs :: WS.Connection -> IO [Message]
receiveMsgs con = do
  received <- WS.receiveData con
  T.putStrLn $ "Server sent: " <> decodeUtf8 received
  case decode $ fromStrict received :: Maybe [Message] of
    Just msgs -> pure msgs
    Nothing -> throwString "Couldn't decode the message from the server"
  
sendMessage :: ButtPlugConnection -> Message -> IO ()
sendMessage con msg = sendMessages con [msg]

sendMessages :: ButtPlugConnection -> [Message] -> IO ()
sendMessages (WebSocketConnection con) msgs = do
  liftIO $ WS.sendTextData con (encode msgs)

close :: ButtPlugM ()
close  = do
  (WebSocketConnection con) <- getConnection
  liftIO $ WS.sendClose con ("Bye!" :: Text)

getConnection :: ButtPlugM ButtPlugConnection
getConnection = ask

data ButtPlugConnection = WebSocketConnection { con  :: WS.Connection
                                              }

type ButtPlugM a = ReaderT ButtPlugConnection IO a 

data ButtPlugApp = ButtPlugApp 
  { handleDeviceAdded :: DeviceAddedFields -> ButtPlugM ()
  }

runButtPlugM :: ButtPlugConnection -> ButtPlugM a -> IO a
runButtPlugM con bpm = runReaderT bpm con

runButtPlugWSApp :: String
               -> Int
               -> ButtPlugApp
               -> IO ()
runButtPlugWSApp host port (ButtPlugApp handleDeviceAdded) = 
  withSocketsDo $ WS.runClient host port "/" \wsCon -> do
    putStrLn "Connected!"

    let
        con = WebSocketConnection wsCon
        reqServerInfo = RequestServerInfo $
          RequestServerInfoFields { id = 1
                                  , clientName = "haskell"
                                  , messageVersion = clientMessageVersion }
        reqDeviceList = RequestDeviceList $ RequestDeviceListFields 1
        startScanning = StartScanning $ StartScanningFields 1

    -- WS.sendTextData con (encode [reqServerInfo, reqDeviceList, startScanning])
    sendMessages con [reqServerInfo, reqDeviceList, startScanning]

    _ <- forkIO $ forever $ handleReceivedData wsCon

    T.putStrLn "Press enter to exit"
    _ <- getLine
    liftIO $ WS.sendClose wsCon ("Bye!" :: Text)

    pure ()
  where

    handleReceivedData :: WS.Connection -> IO ()
    handleReceivedData con = do
      T.putStrLn "Listening for messages from server"
      forever $ receiveMsgs con >>= handleMsgs con

    handleMsgs :: WS.Connection -> [Message] -> IO ()
    handleMsgs con msgs = do
      T.putStrLn "Handling received messages\n"
      traverse_ (handleMsg con) msgs

    --handleMsg :: ButtPlugConnection -> Message -> IO ()
    handleMsg :: WS.Connection -> Message -> IO ()
    handleMsg con = \case
      (DeviceAdded deviceAddedFields) -> do 
        _ <- forkIO $ runButtPlugM  (WebSocketConnection con)
                                    (handleDeviceAdded deviceAddedFields)
        pure ()

      msg -> T.putStrLn $ "Received unhandled message: " <> (T.pack $ show msg)


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
