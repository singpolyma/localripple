module Main (main) where

import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..))
import Control.Error (headMay)
import System.IO (hPutStrLn, stderr)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (getWorkingDirectory)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Network.Wai.Dispatch
import Routes
import Websocket

errLn :: String -> IO ()
errLn = hPutStrLn stderr

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

main :: IO ()
main = do
	args <- getArgs
	let root = fmap addTrailingSlash (parseAbsoluteURI =<< headMay args)
	main' root args
	where
	main' (Just root@(URI {uriAuthority = Just _})) (_:port:_) = do
		cwd <- getWorkingDirectory
		ws <- wsManager "s1.ripple.com" 443 "/"
		run (read port) $
			logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
			dispatch (staticRoot cwd) $ routes root ws         -- Do routing
	main' root@(Just (URI {uriAuthority = Just _})) _ =
		main' root [undefined,"3000"]
	main' _ _ = errLn "Usage: ./Main <Root URI> <port>"
