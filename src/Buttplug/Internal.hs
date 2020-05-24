{-# LANGUAGE OverloadedStrings #-}
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
                , ButtPlugM
                , ButtPlugApp(..)
                , runButtPlugM
                , runButtPlugWSApp
                , getConnection
                , vibrateOnlyMotor
                )-} 

import           Data.Foldable                ( traverse_, for_ )
import           Control.Monad                ( forever )
import           UnliftIO.Concurrent           ( forkIO, ThreadId )
import           UnliftIO.Async
import           UnliftIO.STM
import           Data.Void
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Data.Text.Encoding           ( decodeUtf8 )
import           Network.Socket               ( withSocketsDo )
import           GHC.Generics
import           Data.Text                    ( Text )
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
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
                                              , genericParseJSON
                                              , encode
                                              , decode)
import qualified Data.Map.Strict              as Map
import           Data.Map.Strict              ( Map )
import           Data.ByteString.Lazy         ( fromStrict )
import           UnliftIO.Exception           ( throwString )

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

-- todo: refactor to be ButtPlugConnection -> IO [Message]
receiveMsgs :: WS.Connection -> IO [Message]
receiveMsgs con = do
  received <- WS.receiveData con
  --T.putStrLn $ "Server sent: " <> decodeUtf8 received
  case decode $ fromStrict received :: Maybe [Message] of
    Just msgs -> pure msgs
    Nothing -> throwString "Couldn't decode the message from the server"


-- the message id is decided at the last second in handleOutGoing, so we take a function
-- from messageid to message.
-- we return the response from the server
sendMessage :: (Int -> Message) -> ButtPlugM (Async Message)
sendMessage msgNoId = do

  liftIO $ putStrLn "sendMessage called"
  msgId <- getNextMsgId

  let msg = msgNoId msgId

  outGoing <- getOutgoingChan
  atomically $ writeTChan outGoing msg

  waitForResponse msgId

  where

waitForResponse :: Int -> ButtPlugM (Async Message)
waitForResponse msgId = do

  awaitingResponseVar <- getAwaitingResponses

  async do
    tmvarResponse <- atomically do
      awaitingResponseMap <- readTVar awaitingResponseVar
      maybe retrySTM pure $ Map.lookup msgId awaitingResponseMap

    atomically do
      response <- takeTMVar tmvarResponse
      modifyTVar awaitingResponseVar (Map.delete msgId)
      pure response

getNextMsgId :: ButtPlugM Int
getNextMsgId = do
  var <- clientNextMsgId <$> ask
  atomically do
    next <- readTVar var
    modifyTVar var (+1)
    pure next

-- Should return a message containing the server info
handshake :: ButtPlugM (Async Message)
handshake = sendMessage \msgId ->
    RequestServerInfo { msgId = msgId 
                      -- TODO this should be passed in by the user
                      , msgClientName = "Buttplug-hs"
                      , msgMessageVersion = 1 
                      }

startScanning :: ButtPlugM (Async Message)
startScanning = do
  sendMessage \msgId -> StartScanning { msgId = msgId }

handleIO :: ButtPlugM Void
handleIO = do
  liftIO $ putStrLn "handleIO called"
  client <- ask
  concurrently handleIncoming handleOutGoing

  forever $ pure ()

handleOutGoing :: ButtPlugM Void
handleOutGoing = do
  liftIO $ putStrLn "handleOutGoing called"
  Client { clientCon = con
         , clientOutgoingQ = outGoing
         , clientAwaitingResponse = awaitingResponse } <- ask


  forever do
    -- block until we have a message to send
    msg <- atomically $ readTChan outGoing
    atomically do
      var <- newEmptyTMVar
      modifyTVar' awaitingResponse $ Map.insert (msgId msg) var

    liftIO do putStrLn $ "Sending: "
              print msg
              sendData (encode [msg]) con

  where
    sendData d = \case
      WebSocketConnection con -> WS.sendTextData con d

-- Recieve incoming messages from the connector. Any messages which are responses to
-- messages sent earlier are stored for retrieval by the asyncs that expect them. 
-- All others are broadcast on the clientIncomingQ to be handled by a provided handler
handleIncoming :: ButtPlugM Void
handleIncoming = do
  liftIO $ putStrLn "handleIncoming called"
  client@Client { clientCon = WebSocketConnection con
                , clientOutgoingQ = outGoing
                , clientAwaitingResponse = awaitingResponse
                , clientIncomingQ = incoming } <- ask

  forever do
    messages <- liftIO $ receiveMsgs con
    for_ messages \msg -> do

      liftIO $ print msg

      -- if there is a thread awaiting this message as a response, send the
      -- response to the thread and ignore it. Otherwise, pass it to the 
      -- provided handler
      shouldHandle <- atomically do
        awaitingResponseMap <- readTVar awaitingResponse

        case Map.lookup (msgId msg) awaitingResponseMap of
          Just var -> putTMVar var msg >> pure False
          Nothing -> pure True

      if shouldHandle
      then liftIO do putStrLn "passing message to handler"
                     atomically $ writeTChan incoming msg
      else liftIO $ putStrLn "passed reply to the thread that was expecting it"

getOutgoingChan = clientOutgoingQ <$> ask



close :: ButtPlugM ()
close = do
  (WebSocketConnection con) <- getConnection
  liftIO $ WS.sendClose con ("Bye!" :: Text)


getConnection :: ButtPlugM ButtPlugConnection
getConnection = clientCon <$> ask


stopDevice :: Device -> ButtPlugM (Async Message)
stopDevice dev@(Device {deviceName=dName, deviceIndex=dIdx})
  -- temporary hack until server handles StopDeviceCmd for youou correctly
  | "Youou" `T.isInfixOf` dName = stopYouou dev
  | otherwise = do
      let msg = \msgId -> StopDeviceCmd { msgId = msgId, msgDeviceIndex = dIdx }
      sendMessage msg -- todo: proper exception handling
  where
    stopYouou :: Device -> ButtPlugM (Async Message)
    stopYouou dev = vibrateOnlyMotor dev $ 1/255


vibrateOnlyMotor :: Device -> Double -> ButtPlugM (Async Message)
vibrateOnlyMotor (Device {deviceIndex = dIdx}) speed = do
  let msg = \mid -> VibrateCmd { msgId = 1
                               , msgDeviceIndex = dIdx
                               , msgSpeeds = [MotorVibrate { index = 0
                                                           , speed = speed }]
                               }
  sendMessage msg


-- Each time the client sends a message expecting a response, it registers that expectation in this map.
-- We map the message id of the expected response to a channel to the thread waiting on that response.
-- After notifying the thread of the arrival of the message, we remove the entry from the map.
type AwaitingResponseMap = (Map Int (TMVar Message))

getAwaitingResponses :: ButtPlugM (TVar AwaitingResponseMap)
getAwaitingResponses = clientAwaitingResponse <$> ask

data Client = Client { clientCon :: ButtPlugConnection
                     , clientAwaitingResponse :: TVar AwaitingResponseMap
                     , clientNextMsgId :: TVar Int
                     , clientOutgoingQ :: TChan Message
                     , clientIncomingQ :: TChan Message
                     }


data ButtPlugConnection = WebSocketConnection { con  :: WS.Connection
                                              }


data Connector = WebSocketConnector { wsConnectorHost :: String
                                    , wsConnectorPort :: Int 
                                    }


type ButtPlugM a = ReaderT Client IO a


data ButtPlugApp = ButtPlugApp 
  { handleDeviceAdded :: Device -> ButtPlugM ()
  }


runButtPlugM :: Client -> ButtPlugM a -> IO a
runButtPlugM client bpm = runReaderT bpm client

newClient :: ButtPlugConnection -> IO Client
newClient con = atomically $
  Client <$> pure con
         <*> newTVar mempty
         <*> newTVar 1
         <*> newTChan
         <*> newTChan


runButtPlugApp :: Connector
               -> ButtPlugApp
               -> IO ()
runButtPlugApp (WebSocketConnector host port) (ButtPlugApp handleDeviceAdded) = 
  withSocketsDo $ WS.runClient host port "/" \wsCon -> do
    let con = WebSocketConnection wsCon
    
    client <- newClient con

    putStrLn "Connected to the Buttplug server."

    runButtPlugM client $ withWorker handleIO $ do
      liftIO $ T.putStrLn "Press enter to exit"
      aServerInfo <- handshake
      startScanning
      liftIO getLine
      close

-- From https://mazzo.li/posts/threads-resources.html
withWorker :: MonadUnliftIO m
           => m Void -- ^ Worker to run
           -> m a
           -> m a
withWorker worker cont = either absurd Prelude.id <$> race worker cont


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


