{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NonCore where

import           UnliftIO.Concurrent           ( forkIO, ThreadId )
import           UnliftIO.Async
import           UnliftIO.STM
import           Data.Void
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Exception
import qualified Data.Map.Strict              as Map
import           Data.Map.Strict              ( Map )
import qualified Network.WebSockets           as WS
import qualified Data.Text                    as T
import           Data.Text                    ( Text )
import qualified Data.Text.IO                 as T
import           Network.Socket               ( withSocketsDo )
import           Data.Aeson                   ( decode
                                              , encode )
import           Data.ByteString.Lazy         ( fromStrict )
import           UnliftIO.Exception           ( throwString )
import           Control.Monad                ( forever )
import           Data.Foldable                ( traverse_, for_ )

import           Buttplug
import qualified Buttplug.Devices             as Dev
import           Buttplug.Devices             ( Device(..) )

type ButtPlugM a = ReaderT Client IO a

data ButtPlugApp = ButtPlugApp 
  { handleDeviceAdded :: Device -> ButtPlugM ()
  }

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

runButtPlugM :: Client -> ButtPlugM a -> IO a
runButtPlugM client bpm = runReaderT bpm client

newClient :: ButtPlugConnection -> IO Client
newClient con = atomically $
  Client     con
         <$> newTVar mempty
         <*> newTVar 1
         <*> newTChan
         <*> newTChan


runButtPlugApp :: Connector
               -> ButtPlugApp
               -> IO ()
runButtPlugApp (WebSocketConnector host port) app = 
  withSocketsDo $ WS.runClient host port "/" \wsCon -> do
    let con = WebSocketConnection wsCon
    client <- newClient con

    putStrLn "Connected to the Buttplug server."

    runButtPlugM client $ withWorker (handleIO app) do
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
withWorker worker cont = either absurd id <$> race worker cont

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

  liftIO $ putStrLn $ "in sendMessage, msg is:\n" <> show msg


  outGoing <- getOutgoingChan
  atomically $ writeTChan outGoing msg

  getResponse msgId

  where

getResponse :: Int -> ButtPlugM (Async Message)
getResponse msgId = do

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


handleIO :: ButtPlugApp -> ButtPlugM Void
handleIO (ButtPlugApp handleDeviceAdded) = do
  liftIO $ putStrLn "handleIO called"
  incoming <- clientIncomingQ <$> ask
  withWorker (concurrently0 handleIncoming handleOutGoing)
             $ forever $ atomically (readTChan incoming) >>= handleNewMessages
  where
    handleNewMessages = \case
      DeviceAdded _ name idx msgs -> do
        handleDeviceAdded $ Device name idx msgs
      _ -> do
        liftIO $ putStrLn "Discarding unhandled message"


-- concurrently do actions that loop infinitely unless terminated by exception,
-- which will be re-thrown
concurrently0 :: MonadUnliftIO m => m Void -> m Void -> m Void
concurrently0 m n = fmap undefined $ concurrently m n


handleOutGoing :: ButtPlugM Void
handleOutGoing = do
  liftIO $ putStrLn "handleOutGoing called"
  Client { clientCon = con
         , clientOutgoingQ = outGoing
         , clientAwaitingResponse = awaitingResponse } <- ask

  forever do
    -- block until we have a message to send
    msg <- atomically $ readTChan outGoing
    -- record that we expect a response with the same message id
    -- the response will be placed in the created TMvar
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
-- All others are sent on the clientIncomingQ to be handled in handleIO
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

      liftIO $ putStrLn "received message:"
      liftIO $ print msg

      -- If the message is a response to a message we sent, send the
      -- response to the thread that sent it.
      -- Otherwise, pass it on to be handled.
      --
      -- todo: this is gross, not sure how to refactor
      shouldHandle <- atomically do
        awaitingResponseMap <- readTVar awaitingResponse

        case Map.lookup (msgId msg) awaitingResponseMap of
          Just var -> putTMVar var msg >> pure False
          Nothing -> pure True

      if shouldHandle
      then liftIO do putStrLn "passing message to handler"
                     atomically $ writeTChan incoming msg
      else liftIO $ putStrLn "passed reply to the thread that was expecting it"


close :: ButtPlugM ()
close = do
  (WebSocketConnection con) <- getConnection
  liftIO $ WS.sendClose con ("Bye!" :: Text)


getConnection :: ButtPlugM ButtPlugConnection
getConnection = clientCon <$> ask


getOutgoingChan :: ButtPlugM (TChan Message)
getOutgoingChan = clientOutgoingQ <$> ask


data ServerError = ServerError { errorMessage :: Text
                               , errorCode :: ErrorCode 
                               }
                               deriving (Show)

instance Exception ServerError

expectOK :: Async Message -> ButtPlugM ()
expectOK aMsg = wait aMsg >>= \case
  Ok _ -> pure ()
  err@(Error _ msg code) -> liftIO do
    putStrLn "Error from the server:"
    print err
    throwIO $ ServerError msg code
  msg -> liftIO do
    putStrLn "Got an unexpected response! This is a bug"
    print msg


stopDevice :: Device -> ButtPlugM ()
stopDevice dev@(Device {deviceName=dName, deviceIndex=dIdx})
  -- temporary hack until server handles StopDeviceCmd for youou correctly
  | "Youou" `T.isInfixOf` dName = stopYouou   dev
  | otherwise                   = stopRegular dev
  where
    stopYouou :: Device -> ButtPlugM ()
    stopYouou dev = vibrateAllMotors dev $ 1/255

    stopRegular :: Device -> ButtPlugM ()
    stopRegular dev =
      sendMessage (\msgId -> StopDeviceCmd { msgId = msgId, msgDeviceIndex = dIdx })
      >>= expectOK


vibrateAllMotors :: Device -> Double -> ButtPlugM ()
vibrateAllMotors dev speed =

  case Map.lookup Dev.VibrateCmd (deviceMessages dev) of
    Just (Dev.MessageAttributes (Just featureCount)) -> do
      let msg = \msgId -> 
            VibrateCmd { msgId = msgId
                       , msgDeviceIndex = deviceIndex dev
                       , msgSpeeds = [MotorVibrate idx speed | idx <- [0..featureCount-1]]
                       }
      sendMessage msg >>= expectOK
    _ -> throwString "This device can't vibrate!"
