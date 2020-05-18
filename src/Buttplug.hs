{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buttplug where

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
                                     , SumEncoding(..)
                                     , genericParseJSON
                                     , encode
                                     , decode)
-- import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
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
               -- enumeration messages
             | StartScanning { id :: Int }
             | RequestDeviceList { id :: Int }
             | DeviceList { id :: Int
                          , devices :: [ Device ]
                          }
             | DeviceAdded { id :: Int
                           , deviceName :: Text
                           , deviceIndex :: Int
                           , deviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
                           }
               -- generic device messages
             | VibrateCmd { id :: Int
                          , deviceIndex :: Int
                          , speeds :: [ MotorVibrate ]
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

vibrateOnlyMotor :: Int -> Double -> ButtPlugM ()
vibrateOnlyMotor deviceIdx speed = do
  let msg = VibrateCmd { id = 1
                       , deviceIndex = deviceIdx
                       , speeds = [MotorVibrate { index = 0
                                                , speed = speed }]
                       }
  con <- getConnection
  liftIO $ sendMessage con msg

data ButtPlugConnection = WebSocketConnection { con  :: WS.Connection
                                              }

type ButtPlugM a = ReaderT ButtPlugConnection IO a 

data ButtPlugApp = ButtPlugApp 
  { handleDeviceAdded :: Device -> ButtPlugM ()
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
        reqServerInfo = RequestServerInfo
                          { id = 1
                          , clientName = "haskell"
                          , messageVersion = clientMessageVersion }
        reqDeviceList = RequestDeviceList 1
        startScanning = StartScanning 1

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
      (DeviceAdded _msgId name idx allowedMsgs) -> do
        let dev = Dev.Device name idx allowedMsgs
        _ <- forkIO $ runButtPlugM  (WebSocketConnection con)
                                    (handleDeviceAdded dev)
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
