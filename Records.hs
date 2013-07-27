module Records where

import Data.Text (Text)

data Liquidity = Liquidity {
		alts :: [Alt],
		pfForm :: [PathFindForm]
	}

data Alt = Alt {
		alt :: String
	} deriving (Eq)

data PathFindForm = PathFindForm {
		from :: Text,
		fromErr :: [ErrorMessage],
		to :: Text,
		toErr :: [ErrorMessage],
		amount :: Text,
		amountErr :: [ErrorMessage],
		currency :: Text,
		currencyErr :: [ErrorMessage]
	} deriving (Eq)

data ErrorMessage = ErrorMessage {
		errorMessage :: String
	} deriving (Eq)
