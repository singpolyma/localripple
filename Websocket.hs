module Websocket (rippleWS, RippleError, wsManager, syncCall) where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.Foldable (for_)
import Control.Concurrent.STM.TMVar (putTMVar, newEmptyTMVar, takeTMVar, TMVar)
import Control.Proxy.Concurrent (Input, Output, spawn, recv, send, Buffer(Unbounded), forkIO, atomically)
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson

-- TODO extract error message
data RippleError = RippleError
	deriving (Show, Eq)

syncCall :: Input a -> (TMVar r -> a) -> IO r
syncCall chan msg = do
	r <- atomically $ newEmptyTMVar
	void $ atomically $ send chan (msg r)
	atomically $ takeTMVar r

wsManager :: (Aeson.ToJSON i, Aeson.FromJSON o) =>
	String -> Int -> String -> IO (Input (i, TMVar (Either RippleError o)))
wsManager host port path = do
	(msgIn, msgOut) <- spawn Unbounded
	void $ forkIO (rippleWS host port path >>= next msgOut)
	return msgIn
	where
	next chan (input, output) = atomically (recv chan) >>= flip for_ (go chan input output)

	go chan input output (i, r) = do
		sent <- atomically $ send input i
		if sent then do
			-- Sent worked, fork recieve thread
			void $ forkIO $ atomically $
				-- If connection drops between send and receive, we return error
				maybe (putTMVar r (Left RippleError)) (putTMVar r) =<< recv output
			next chan (input, output)
			else do
				-- Reconnect on send failure
				(input', output') <- rippleWS host port path
				-- Resend, since send failed
				go chan (input' `asTypeOf` input) (output' `asTypeOf` output) (i, r)

newtype Result a = Result (Either RippleError a)

instance (Aeson.FromJSON a) => Aeson.FromJSON (Result a) where
	parseJSON (Aeson.Object o) = Result <$> do
		status <- o .: T.pack "status"
		result <- o .: T.pack "result"
		case (status, result) of
			("success", Aeson.Object o') ->
				fmap Right (Aeson.parseJSON (Aeson.Object o'))
			_ -> fail "Invalid Ripple Result"
	parseJSON _ = fail "Ripple Result is alsways a JSON object"

wsReceiveJSON :: (WS.TextProtocol p, Aeson.FromJSON j) => WS.WebSockets p (Either RippleError j)
wsReceiveJSON = do
	x <- WS.receiveData
	case Aeson.decode x of
		Just (Result v) -> return v
		Nothing -> return (Left RippleError)

rippleWS' :: (Aeson.ToJSON i, Aeson.FromJSON o) => Output i -> Input (Either RippleError o) -> WS.WebSockets WS.Hybi10 ()
rippleWS' inputOut outputIn = do
	WS.getSink >>= liftIO . void . forkIO . sendLoop
	receiveLoop
	where
	sendLoop sink = atomically (recv inputOut) >>= flip for_ (\x -> do
			WS.sendSink sink $ WS.textData $ Aeson.encode x
			sendLoop sink
		)

	receiveLoop = do
		result <- wsReceiveJSON >>= liftIO . atomically . send outputIn
		when result receiveLoop

-- | If the connection terminates, the mailboxes will start to error on use
rippleWS :: (Aeson.ToJSON i, Aeson.FromJSON o) => String -> Int -> String -> IO (Input i, Output (Either RippleError o))
rippleWS host port path = do
	(inputIn, inputOut) <- spawn Unbounded
	(outputIn, outputOut) <- spawn Unbounded
	void $ forkIO $ WS.connect host port path (rippleWS' inputOut outputIn)
	return (inputIn, outputOut)
