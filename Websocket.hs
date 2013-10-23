module Websocket (rippleWS, RippleError, wsManager, syncCall) where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Data.Foldable (traverse_)
import Control.Exception (finally)
import Control.Concurrent.STM.TMVar (putTMVar, newEmptyTMVar, takeTMVar, TMVar)
import Pipes.Concurrent (Input, Output, spawn, spawn', recv, send, Buffer(Unbounded, Single), forkIO, atomically)
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson

-- TODO extract error message
data RippleError = RippleError
	deriving (Show, Eq)

syncCall :: Output a -> (TMVar r -> a) -> IO r
syncCall chan msg = do
	r <- atomically $ newEmptyTMVar
	void $ atomically $ send chan (msg r)
	atomically $ takeTMVar r

-- Spawn rippleWS with Single so that we can only lose one send item
-- Which we account for as a connection failure between send and receive
wsManager :: (Aeson.ToJSON i, Aeson.FromJSON o) =>
	String -> Int -> String -> IO (Output (i, TMVar (Either RippleError o)))
wsManager host port path = do
	(msgIn, msgOut) <- spawn Unbounded
	void $ forkIO (rippleWS host port path Single Unbounded >>= next msgOut)
	return msgIn
	where
	next chan (input, output) = atomically (recv chan) >>= traverse_ (go chan input output)

	go chan input output (i, r) = do
		sent <- atomically $ send input i
		if sent then do
			-- Send worked, fork recieve thread
			void $ forkIO $ atomically $ recv output >>=
				-- If connection drops between send and receive, we return error
				maybe (putTMVar r (Left RippleError)) (putTMVar r)
			next chan (input, output)
			else do
				-- Reconnect on send failure
				(input', output') <- rippleWS host port path Single Unbounded
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

unResult :: Maybe (Result a) -> Either RippleError a
unResult (Just (Result x)) = x
unResult _ = Left RippleError

wsReceiveJSON :: (Aeson.FromJSON j) => WS.Connection -> IO (Maybe j)
wsReceiveJSON = fmap Aeson.decode . WS.receiveData

wsSendJSON :: (Aeson.ToJSON j) => WS.Connection -> j -> IO ()
wsSendJSON conn = WS.sendTextData conn . Aeson.encode

-- On any exception, we stop accepting new messages
-- We stop and the connection is closed if no one is listening anymore
rippleWS' :: (Aeson.ToJSON i, Aeson.FromJSON o) => Input i -> Output (Either RippleError o) -> IO () -> WS.Connection -> IO ()
rippleWS' inputOut outputIn sealInput connection = do
	-- Stop accepting input if connection fails
	void $ forkIO (sendLoop `finally` sealInput)
	receiveLoop `finally` sealInput
	where
	sendLoop = atomically (recv inputOut) >>= traverse_ (\x -> do
			wsSendJSON connection x
			sendLoop -- Loop until recv fails
		)

	receiveLoop = do
		msg <- wsReceiveJSON connection
		result <- atomically $ send outputIn $ unResult msg
		when result receiveLoop -- Loop until send fails

-- | If the connection terminates, the mailboxes will start to error on use
rippleWS :: (Aeson.ToJSON i, Aeson.FromJSON o) => String -> Int -> String -> Buffer i -> Buffer (Either RippleError o) -> IO (Output i, Input (Either RippleError o))
rippleWS host port path bufferIn bufferOut = do
	(inputIn, inputOut, inputSeal) <- spawn' bufferIn
	(outputIn, outputOut) <- spawn bufferOut
	void $ forkIO $ WS.runClient host port path (rippleWS' inputOut outputIn (atomically inputSeal))
	return (inputIn, outputOut)
