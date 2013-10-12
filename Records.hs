module Records where

import Data.Text.Buildable
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

data Liquidity = Liquidity {
		result :: [Result],
		pfForm :: Html
	}

data Result = Result {
		from :: String,
		to :: String,
		alts :: [Alt]
	}

data Alt = Alt {
		alt :: String
	} deriving (Eq)

data ErrorMessage = ErrorMessage {
		errorMessage :: String
	} deriving (Eq)
