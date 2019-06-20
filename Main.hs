{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude
import SimpleCmdArgs

--import Data.Functor ((<&>))
import qualified Data.Text as T
import Network.HTTP.Directory
import System.FilePath.Glob (compile, match)

import Paths_findhttp (version)

main :: IO ()
main = do
  mgr <- httpManager
  simpleCmdArgs (Just version) "find for http"
    "Find files from an http \"directory\"" $
    findHttp mgr Nothing <$> optional nameOpt <*> strArg "URL"
  where
    nameOpt :: Parser String
    nameOpt = strOptionWith 'n' "name" "GLOB" "Limit files to glob matches"

findHttp :: Manager -> Maybe Text -> Maybe String -> String -> IO ()
findHttp mgr mprefix mname url = do
  fs <- httpDirectory mgr url
  mapM_ display fs
  where
    display :: Text -> IO () 
    display f =
      -- optimisation: assume dirs don't contain '.'
      if T.any (== '.') f then dispFile f
      else findHttp mgr (mprefix <> Just f <> Just "/") mname $ url </> T.unpack f

    glob = maybe (const True) (match . compile) mname . T.unpack

    dispFile :: Text -> IO ()
    dispFile f =
      when (glob f) $ putStrLn $ prefix f

    prefix = (fromMaybe "" mprefix <>) 
