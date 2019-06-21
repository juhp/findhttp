{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude
import SimpleCmdArgs

--import Data.Functor ((<&>))
import qualified Data.Text as T
import Network.HTTP.Directory
import System.Directory
import System.FilePath.Glob (compile, match)

import Paths_findhttp (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "find for http"
  "Find files from an http \"directory\"" $
  listFiles <$> optional nameOpt <*> strArg "URL/DIR"
  where
    nameOpt :: Parser String
    nameOpt = strOptionWith 'n' "name" "GLOB" "Limit files to glob matches"

listFiles :: Maybe String -> String -> IO ()
listFiles mname dir =
  if isHttp dir then do
    mgr <- httpManager
    findHttp mgr Nothing mname $ makeDir dir
    else
    findDir mname dir

findDir :: Maybe String -> String -> IO ()
findDir mname dir = do
  fs <- sort <$> listDirectory dir
  mapM_ display fs
  where
    display :: String -> IO ()
    display f = do
      let file = dir </> f
      isdir <- doesDirectoryExist file
      when (glob f) $ putStrLn $ T.pack file <> (if isdir then "/" else "")
      when isdir $ findDir mname file

    glob = maybe (const True) (match . compile) mname

findHttp :: Manager -> Maybe Text -> Maybe String -> String -> IO ()
findHttp mgr mprefix mname url = do
  fs <- httpDirectory mgr url
  mapM_ display fs
  where
    display :: Text -> IO () 
    display f = do
      when (glob f) $ putStrLn $ prefix f
      -- optimisation: assume dirs don't contain '.'
      unless (T.any (== '.') f) $
        findHttp mgr (mprefix <> Just f <> Just "/") mname $ url </> T.unpack f <> "/"

    glob = maybe (const True) (match . compile) mname . T.unpack

    prefix = (fromMaybe "" mprefix <>) 

makeDir :: String -> String
makeDir path =
  if last path == '/' then path else path <> "/"
