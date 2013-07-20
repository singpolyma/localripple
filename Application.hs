{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Application where

import Prelude (show)
import BasicPrelude hiding (show)
import Network.Wai (Request(..), Response(..), Application)
import Network.HTTP.Types (ok200, badRequest400, Status, ResponseHeaders)
import Network.Wai.Util (stringHeaders)
import Data.Base58Address (RippleAddress)
import Control.Error (readMay, note, hush, assertZ)
import Network.URI (URI(..))
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Control.Proxy.Concurrent as PC

import Records
import MustacheTemplates
import PathFind
import Amount
import Websocket
#include "PathHelpers.hs"

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

responseTextBuilder :: Status -> ResponseHeaders -> TL.Builder -> Response
responseTextBuilder s h = ResponseBuilder s h . Blaze.fromLazyText . TL.toLazyText

requiredParam :: (Eq k) => e -> (v -> Maybe v) -> (v -> Either e a) -> k -> [(k, v)] -> Either e a
requiredParam notPresent maybePresent parser k =
	parser <=< note notPresent . (maybePresent <=< lookup k)

optionalParam :: (Eq k) => (v -> Maybe v) -> (v -> Either e a) -> k -> [(k, v)] -> Maybe a
optionalParam maybePresent parser k =
	hush . parser <=< maybePresent <=< lookup k

blankNotPresent :: (Eq m, Monoid m) => m -> Maybe m
blankNotPresent t
	| mempty == t = Nothing
	| otherwise = Just t

nothingNotPresent :: Maybe v -> Maybe (Maybe v)
nothingNotPresent Nothing = Nothing
nothingNotPresent x = Just x

readUtf8BS :: (Read a) => ByteString -> Maybe a
readUtf8BS = readMay . T.unpack <=< (hush . T.decodeUtf8')

assertV :: (MonadPlus m) => (v -> Bool) -> v -> m v
assertV p v = do
	assertZ (p v)
	return v

liquidityCheck :: URI -> (PC.Input PathFindRequest, PC.Output (Either RippleError PathFindResponse)) -> Application
liquidityCheck _ (sendWS, recvWS) req
	| null (queryString req) =
		return $ responseTextBuilder ok200 headers (viewLiquidityCheck htmlEscape $ Liquidity { err = "", from = "", to = "", alts = [] })
	| otherwise =
		-- TODO: this error checking is the worst
		-- Also, websocket reconnect logic
		case pathfind of
			Left e ->
				return $ responseTextBuilder badRequest400 headers (viewLiquidityCheck htmlEscape $ Liquidity { err = e, from = "", to = "", alts = [] })
			Right pf -> do
				worked <- liftIO $ PC.atomically $ PC.send sendWS pf
				if (not worked) then
					return $ responseTextBuilder badRequest400 headers (viewLiquidityCheck htmlEscape $ Liquidity { err = "Websocket is down", from = "", to = "", alts = [] })
					else do
						resp <- liftIO $ PC.atomically $ PC.recv recvWS
						case resp of
							Just (Right (PathFindResponse alts _)) -> do
								let Right f = from
								let Right t = to
								return $ responseTextBuilder ok200 headers (viewLiquidityCheck htmlEscape $ Liquidity { err = "", alts = map (\(Alternative amnt) -> Alt (show amnt)) alts, from = show f, to = show t})
							Just (Left _) ->
								return $ responseTextBuilder badRequest400 headers (viewLiquidityCheck htmlEscape $ Liquidity { err = "Ripple request error", from = "", to = "", alts = [] })
							Nothing ->
								return $ responseTextBuilder badRequest400 headers (viewLiquidityCheck htmlEscape $ Liquidity { err = "Websocket is down", from = "", to = "", alts = [] })
	where
	pathfind = PathFindRequest <$> from <*> to <*>
		(Amount <$> fmap realToFrac amount <*> (Currency <$> currency <*> to))

	from :: Either String RippleAddress
	from = requiredParam "From address required"
		(nothingNotPresent <=< fmap blankNotPresent)
		(note "Invalid from address" . (readUtf8BS =<<))
		(fromString "from") (queryString req)

	to :: Either String RippleAddress
	to = requiredParam "To address required"
		(nothingNotPresent <=< fmap blankNotPresent)
		(note "Invalid to address" . (readUtf8BS =<<))
		(fromString "to") (queryString req)

	amount :: Either String Double
	amount = requiredParam "Amount required"
		(nothingNotPresent <=< fmap blankNotPresent)
		(note "Invalid amount" . (readUtf8BS =<<))
		(fromString "amount") (queryString req)

	currency :: Either String (Char,Char,Char)
	currency = fmap (\[a,b,c] -> (a,b,c)) $ requiredParam "Currency required"
		(nothingNotPresent <=< fmap blankNotPresent)
		(note "Invalid currency" . (assertV (\x -> length x == 3) <=< ((fmap T.unpack . hush . T.decodeUtf8') =<<)))
		(fromString "currency") (queryString req)

	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]
