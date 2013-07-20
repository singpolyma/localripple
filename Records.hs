module Records where

data Liquidity = Liquidity {
		err :: String,
		from :: String,
		to :: String,
		alts :: [Alt]
	}

data Alt = Alt {
		alt :: String
	} deriving (Eq)
