{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Application (liquidityCheck) where

import Prelude (show)
import BasicPrelude hiding (show)
import Control.Applicative (many)
import Network.Wai (Request(..), Response(..), Application)
import Network.HTTP.Types (ok200, badRequest400, Status, ResponseHeaders, Query)
import Network.Wai.Util (stringHeaders)
import Control.Error (hush, assertZ, MaybeT(..), maybeT, hoistMaybe)
import Network.URI (URI(..))
import Text.Digestive
import qualified Data.Attoparsec.Text as Attoparsec
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

currencyForm :: (Monad m) => Form String m Currency
currencyForm = Currency <$>
	T.pack "currency" .: validate threeLetterCode (string Nothing) <*>
	T.pack "to" .: stringRead "invalid Ripple address" Nothing
	where
	threeLetterCode [a,b,c] = Success (a,b,c)
	threeLetterCode _ = Error "invalid currency code"

amountForm :: (Monad m) => Form String m Amount
amountForm = Amount <$>
	fmap fromDbl (T.pack "amount" .: stringRead "invalid amount" Nothing) <*>
	currencyForm
	where
	fromDbl = realToFrac :: Double -> Rational

pathFindForm :: (Monad m) => Form String m PathFindRequest
pathFindForm = PathFindRequest <$>
	T.pack "from" .: stringRead "invalid Ripple address" Nothing <*>
	T.pack "to" .: stringRead "invalid Ripple address" Nothing <*>
	amountForm

queryKeyParse :: Attoparsec.Parser [T.Text]
queryKeyParse = do
	top <- Attoparsec.takeTill (=='[')
	keys <- many key
	return (top:keys)
	where
	key =
		Attoparsec.many1 (Attoparsec.char '[') *>
		Attoparsec.takeTill (==']')
		<* Attoparsec.many1 (Attoparsec.char ']')

queryFormEnv :: (Monad m) => Query -> Env m
queryFormEnv qs pth = return $ map (TextInput . snd) $ filter ((==pth).fst) qs'
	where
	qs' = mapMaybe (\(k,v) -> case (
			hush (T.decodeUtf8' k) >>= hush . Attoparsec.parseOnly queryKeyParse,
			fmap T.decodeUtf8' v
		) of
			(Just k', Just (Right v')) -> Just (k', v')
			(Just k', Nothing) -> Just (k', T.empty)
			_ -> Nothing
		) qs

renderPathFindForm :: View String -> PathFindForm
renderPathFindForm view = PathFindForm {
		from = fieldInputText (T.pack "from") view,
		fromErr = map ErrorMessage $ errors (T.pack "from") view,
		to = fieldInputText (T.pack "to") view,
		toErr = map ErrorMessage $ errors (T.pack "to") view,
		amount = fieldInputText (T.pack "amount") view,
		amountErr = map ErrorMessage $ errors (T.pack "amount") view,
		currency = fieldInputText (T.pack "currency") view,
		currencyErr = map ErrorMessage $ errors (T.pack "currency") view
	}

liquidityCheck :: URI -> (PC.Input PathFindRequest, PC.Output (Either RippleError PathFindResponse)) -> Application
liquidityCheck _ (sendWS, recvWS) req
	| null (queryString req) = do
		form <- getForm T.empty pathFindForm
		return $ responseTextBuilder ok200 headers (viewLiquidityCheck htmlEscape $ Liquidity { alts = [], pfForm = [renderPathFindForm form] })
	| otherwise = do
		-- TODO: websocket reconnect logic
		(view, pathfind) <- postForm T.empty pathFindForm (\(_:x) -> queryFormEnv (queryString req) x)

		alts <- liftIO $ maybeT (return []) return $ do
			pf <- hoistMaybe pathfind
			assertZ =<< liftIO (PC.atomically $ PC.send sendWS pf)
			(PathFindResponse alts _) <- MaybeT $ fmap (join . fmap hush) $ PC.atomically $ PC.recv recvWS
			return $ map (\(Alternative amnt) -> Alt $ show amnt) alts

		return $ responseTextBuilder (maybe badRequest400 (const ok200) pathfind) headers (viewLiquidityCheck htmlEscape $ Liquidity { alts = alts, pfForm = [renderPathFindForm view] })
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]
