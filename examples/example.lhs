> {-# LANGUAGE BlockArguments #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE LambdaCase #-}
> 
> import qualified Data.Text.IO             as T
> import           Data.Foldable            (for_)
> import           Control.Monad            (forever)
> import           Control.Exception        (handle)
> import           System.Environment       (getArgs)
> import qualified Network.WebSockets       as WS
> import           Control.Concurrent.Async (concurrently_)
> import           Control.Concurrent       (threadDelay)
> import           Data.Word                (Word32)
>
> import           Buttplug.Core

A `Connector` represents a method of connecting to a Buttplug server, and
contains all of the necessary information required to connect.
A Connector for websockets is included in this
library. You can create your own using the Connector typeclass.

Here we use a websocket connector. If your server is running
somewhere other than localhost:12345, you'll need to pass the port and host as
command line args.

> main :: IO ()
> main = do
>   args <- getArgs
>   let (host, port) = case args of
>         [host, port] -> (host, read port)
>         [host]       -> (host, 12345)
>         []           -> ("localhost", 12345)
>         _            -> error "Too many args!"
>       connector =
>         WebSocketConnector { wsConnectorHost = host
>                            , wsConnectorPort = port }
>       clientName = "Haskell-example-buttplug-client"

`runClient` is responsible for establishing and closing the connection.
We pass it a function which takes a connection and returns an IO action,
which will make use of that connection to send and receive Buttplug messages.

The ConnectorException encapsulates the exceptions that can occur in the course
of a connection to a Buttplug server.

Once we've established a connection, we need to perform a handshake with the
server. A Buttplug handshake involves sending the server a RequestServerInfo
message.
We use the sendMessage function to send messages.
The server will reply with a ServerInfo message.
See https://buttplug-spec.docs.buttplug.io/architecture.html#stages
for details and diagrams.

>   handle handleConnectorException $ runClient connector \con -> do
>     putStrLn "Beginning handshake..."
>     sendMessage con
>       MsgRequestServerInfo { msgId = 1
>                            , msgClientName = clientName
>                            , msgMessageVersion = clientMessageVersion
>                            }

We receive messages using the receiveMsgs function. The server should reply 
with ServerInfo to complete the handshake.

>     receiveMsgs con >>= \case
>       [MsgServerInfo 1 servName msgVersion maxPingTime] -> do
>         T.putStrLn $ "Successfully connected to server \"" <> servName <> "\"!"
>         putStrLn $ "Message version: " <> show msgVersion <>
>                    "\nMax ping time (ms): " <> show maxPingTime

Once we have successfully connected to the server, we ask it to
begin scanning for devices.

>         putStrLn "Requesting device scan"
>         sendMessage con $ MsgStartScanning 2

We now print out any further messages the server sends us, until it
disconnects. The first thing we should see is an "Ok Id=2" in
response to our request to start scanning for devices.
Additionally, the server will send us a message any time a device
connects or disconnects. 
If the server's maxPingTime is set to a value other than 0, we need to
ping it regularly, or it will disconnect us. In that case, we will also see an 
Ok response for each of our pings.

>         concurrently_ (receiveAndPrintMsgs con)
>                       (pingServer maxPingTime con)

This case would indicate a server bug, it's just here for completeness.

>       _ -> putStrLn "Did not receive expected handshake response"
>
>  where
>    handleConnectorException :: ConnectorException -> IO ()
>    handleConnectorException = \case
>      ConnectionFailed e -> do
>        putStrLn "Connection to Buttplug server failed:"
>        putStrLn e
>      UnexpectedConnectionClosed ->
>        putStrLn "Server closed the connection unexpectedly"
>      ConnectionClosedNormally -> putStrLn "Server closed the connection"
>      ReceivedInvalidMessage bs -> do
>        putStrLn "Server sent a message we didn't recognize:"
>        print bs
>      OtherConnectorError err -> putStrLn $ "Connector error:" ++ err
>
>    receiveAndPrintMsgs con = do 
>      putStrLn "(receiving messages)"
>      forever do arr <- receiveMsgs con
>                 for_ arr print

We ping at twice the specified rate to leave ourselves plenty of room.

>    pingServer :: Word32 -> Connection WebSocketConnector -> IO ()
>    pingServer maxPingTime con = case maxPingTime of
>      0 -> pure ()
>      n -> forever do
>        sendMessage con (MsgPing 1)
>        threadDelay $ fromIntegral (n * 1000 `div` 2)
