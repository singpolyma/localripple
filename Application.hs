{-# LANGUAGE CPP #-}
module Application (liquidityCheck) where

import Prelude (show, (.))
import BasicPrelude hiding (show, (.))
import Network.Wai (Request(..), Application)
import Network.HTTP.Types (ok200, badRequest400)
import Network.Wai.Util (stringHeaders, textBuilder)
import Text.Blaze.Html (Html)
import Network.Wai.Digestive (queryFormEnv)
import qualified SimpleForm.Validation as SFV
import qualified SimpleForm as SFW
import SimpleForm.Combined (ShowRead(..), unShowRead, Validation(..), Widget)
import SimpleForm.Digestive.Combined (SimpleForm, getSimpleForm, postSimpleForm, input, input_, fieldset)
import SimpleForm.Render.XHTML5 (render)
import Data.Base58Address (RippleAddress)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Error (hush, MaybeT(..), maybeT, hoistMaybe)
import Text.Digestive hiding (text)
import Network.URI (URI(..))
import qualified Data.Text as T
import qualified Pipes.Concurrent as PC

import Records
import MustacheTemplates
import PathFind
import Amount
import Websocket
#include "PathHelpers.hs"

s :: (IsString s) => String -> s
s = fromString

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

currencyCode :: Currency -> (Char,Char,Char)
currencyCode XRP = ('X','R','P')
currencyCode (Currency code _) = code

codeToText :: (Char,Char,Char) -> Text
codeToText (a,b,c) = T.pack [a,b,c]

textToCode :: Text -> Maybe (Char, Char, Char)
textToCode txt = case T.unpack txt of
	[a,b,c] -> Just (a,b,c)
	_ -> Nothing

threeLetterCode :: (Widget (Char,Char,Char), Validation (Char,Char,Char))
threeLetterCode = (SFW.text . fmap codeToText, SFV.pmap textToCode SFV.text)

cmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
cmap = fmap . fmap

pathFindForm :: (Monad m) => SimpleForm PathFindRequest (Form Html m PathFindRequest)
pathFindForm = do
	from' <- cmap unShowRead $ input_ (s"from") (Just . ShowRead . source_account)
	to' <- cmap unShowRead $ input_ (s"to") (Just . ShowRead . destination_account)
	amount' <- fieldset (s"amount") destination_amount $ do
		quantity' <- input_ (s"quantity") (\(Amount q _) -> Just q)
		currency' <- input (s"currency") (\(Amount _ c) -> Just $ currencyCode c) threeLetterCode mempty
		return $ (.) <$> (Amount <$> quantity') <*> (Currency <$> currency')
	return $ PathFindRequest <$> from' <*> to' <*> (amount' <*> to')

liquidityCheck :: URI -> (PC.Output (PathFindRequest, TMVar (Either RippleError PathFindResponse))) -> Application
liquidityCheck _ ws req
	| null (queryString req) = do
		form <- getSimpleForm render Nothing pathFindForm
		textBuilder ok200 headers (viewLiquidityCheck htmlEscape $ Liquidity { result = [], pfForm = form })
	| otherwise = do
		(form, pathfind) <- postSimpleForm render (return $ queryFormEnv $ queryString req) pathFindForm

		alts <- liftIO $ maybeT (return []) return $ do
			pf <- hoistMaybe pathfind
			(PathFindResponse alts _) <- MaybeT $ fmap hush $ syncCall ws ((,) pf)
			return $ map (\(Alternative amnt) -> Alt $ show amnt) alts

		let result' = case pathfind of
			Just (PathFindRequest from to _) -> [Result (show from) (show to) alts]
			Nothing -> []

		textBuilder (maybe badRequest400 (const ok200) pathfind) headers (viewLiquidityCheck htmlEscape $ Liquidity { result = result', pfForm = form })
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]
