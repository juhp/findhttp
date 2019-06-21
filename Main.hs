{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude
import SimpleCmdArgs

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Network.HTTP.Directory
import System.Directory
import System.FilePath
import System.FilePath.Glob (compile, match)

import Paths_findhttp (version)

data FileType = TypeFile | TypeDir | TypeSymlink
  deriving Eq

main :: IO ()
main =
  simpleCmdArgs (Just version) "find for http"
  "Find files from an http \"directory\"" $
  listFiles <$> depthOpt <*> optional filetypeOpt <*> optional nameOpt <*> strArg "URL/DIR"
  where
    nameOpt :: Parser String
    nameOpt = strOptionWith 'n' "name" "GLOB" "Limit files to glob matches"

    filetypeOpt :: Parser FileType
    filetypeOpt =
      flagWith' TypeFile 'f' "files" "List files only" <|>
      flagWith' TypeDir 'd' "dirs" "List directories only" <|>
      flagWith' TypeSymlink 's' "symlinks" "List symlinks only (not http)"

    depthOpt :: Parser Int
    depthOpt =  optionalWith auto 'm' "maxdepth" "DEPTH" "Maximum search depth (default 10)" 10

listFiles :: Int -> Maybe FileType -> Maybe String -> String -> IO ()
listFiles maxdepth mfiletype mname dir =
  if isHttpUrl dir then do
    mgr <- httpManager
    findHttp mgr maxdepth mfiletype Nothing mname $ makeDir dir
    else
    findDir maxdepth mfiletype mname dir

findDir :: Int -> Maybe FileType -> Maybe String -> String -> IO ()
findDir n _ _ _ | n <= 0 = return ()
findDir maxdepth mfiletype mname dir = do
  fs <- sort <$> listDirectory dir
  mapM_ display fs
  where
    display :: String -> IO ()
    display f = do
      let file = dir </> f
      isdir <- doesDirectoryExist file
      if isdir then do
        when (fileType TypeDir mfiletype && glob f) $
          putStrLn $ T.pack $ addTrailingPathSeparator file
        findDir (maxdepth-1) mfiletype mname file
        else do
        symlink <- pathIsSymbolicLink file
        if symlink then do
          tgt <- getSymbolicLinkTarget file
          when (fileType TypeSymlink mfiletype && glob f) $
            putStrLn $ T.pack $ file <> " -> " <> tgt
          else
          when (fileType TypeFile mfiletype && glob f) $
          putStrLn $ T.pack file

    glob = maybe (const True) (match . compile) mname

fileType :: FileType -> Maybe FileType -> Bool
fileType ftype =
  maybe True (== ftype)

findHttp :: Manager -> Int -> Maybe FileType -> Maybe Text -> Maybe String -> String -> IO ()
findHttp _ n _ _ _ _ | n <= 0 = return ()
findHttp mgr maxdepth mfiletype mprefix mname url = do
  fs <- sort <$> httpDirectory mgr url
  mapM_ (display . T.unpack) fs
  where
    display :: String -> IO ()
    display f = do
      -- optimisation: assume dirs don't contain '.'
      filetype <- if '.' `elem` f then return TypeFile
        else if last f == '/' then return TypeDir
        else httpFileType mgr $ url </> f
      if filetype == TypeDir then do
        let dirname = dropTrailingPathSeparator f
        when (fileType TypeDir mfiletype && glob dirname) $
          putStrLn $ prefix f
        let dir = addTrailingPathSeparator f
        findHttp mgr (maxdepth-1) mfiletype (mprefix <> Just (T.pack dir)) mname $ url </> dir
        else
        when (fileType TypeFile mfiletype && glob f) $
        putStrLn $ prefix f

    glob = maybe (const True) (match . compile) mname

    prefix = (fromMaybe "" mprefix <>) . T.pack

makeDir :: String -> String
makeDir path =
  if last path == '/' then path else path <> "/"

httpFileType :: Manager -> String -> IO FileType
httpFileType mgr url = do
  mredirect <- httpRedirect mgr $ dropTrailingPathSeparator url
  return $ case mredirect of
    Nothing -> TypeFile
    Just uri | uri == B.pack (addTrailingPathSeparator url) -> TypeDir
             | otherwise -> TypeSymlink
