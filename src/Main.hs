{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Main where

-- XXX: fromArchive . toArchive /= id
import Codec.Archive.Zip (toArchive, fromEntry, Archive(zEntries), Entry(eRelativePath))
import Control.Applicative ((<$>))
import Data.Char (isAlphaNum)
import Data.List ((\\), find, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, toList)
import Network.Curl (URLString, curlGetString, CurlCode(CurlOK),
        CurlOption(CurlFollowLocation))
import Network.Curl.Download.Lazy (openLazyURIWithOpts)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Regex.Applicative ((<|>), (*>), (<*), (=~),
        RE, string, anySym, psym, sym, few, some)
import Text.XML.HXT.Core ((>>>), (<+>), when, runX, txt, mkelem,
        processTopDown, replaceChildren, neg, returnA, deep,
        hasName, isElem, getChildren, getText, readString)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


jenkinsDownloadPlugins :: URLString
jenkinsDownloadPlugins = "https://updates.jenkins-ci.org/download/plugins/"

type PluginName = String
type PluginVersion = String

pluginName :: RE Char PluginName
pluginName = few $ psym isAlphaNum <|> sym '-'

pluginVersion :: RE Char PluginVersion
pluginVersion = few $ foldr1 (<|>) $ map sym "0123456789.-"

pluginNames :: RE Char [PluginName]
pluginNames = some $ few anySym *> string "href=\""
              *> pluginName <* string "/\">" <* few anySym

pluginVersions :: RE Char [PluginVersion]
pluginVersions = some $ few anySym *> string ">"
                 *> pluginVersion <* string "</a>" <* few anySym


data PluginInfo = PluginInfo {
    name    :: PluginName,
    version :: PluginVersion,
    sha1    :: String,
    depends :: [PluginName]
  } deriving (Show, Eq, Ord)

getPluginInfo :: PluginName -> PluginVersion -> IO PluginInfo
getPluginInfo name version = do
  hPutStrLn stderr $ "fetching " ++ hpi ++ " ..."
  try 1
  where
    hpi = jenkinsDownloadPlugins ++ name ++ "/" ++
          version ++ "/" ++ name ++ ".hpi"
    try :: Int -> IO PluginInfo
    try 7 = error "giving up :-("
    try n = do
      out <- openLazyURIWithOpts [CurlFollowLocation True] hpi
      case out of
        Left err -> do
          hPutStrLn stderr $ "failed (" ++ err ++ ") trying again ..."
          try $ n + 1
        Right cnt -> do 
          let sha1 =  BS.unpack $ Base16.encode (SHA1.hashlazy cnt)
          depends <- getRuntimeDeps <$> getAllDeps (getPomXml $ toArchive cnt)
          return PluginInfo { .. }

getPluginVersions :: PluginName -> IO [PluginVersion]
getPluginVersions plugin = do
  (rc, html) <- curlGetString (jenkinsDownloadPlugins ++ plugin ++ "/") []
  if rc /= CurlOK then  error $ "not found: " ++ plugin
  else case html =~ pluginVersions of
      Nothing -> error "No versions were found. HTML parsing has failed."
      Just names -> return names

-- XXX: unpack vs. UTF-8
getPomXml :: Archive -> String
getPomXml hpi = LBS.unpack . fromEntry $
                fromMaybe (error "No pom.xml found") pomXml
  where pomXml = find isPomXml (zEntries hpi)
        isPomXml = isSuffixOf "/pom.xml" . eRelativePath


data Dependency = Dependency {
    artifactId :: PluginName,
    scope      :: String,
    groupId    :: String
  } deriving (Show, Eq)

getAllDeps :: String -> IO [Dependency]
getAllDeps pomXml = runX $ readString [] pomXml
  >>> addRuntimeScope >>> deep (isElem >>> hasName "dependency") >>>
  proc d -> do
    artifactId <- value "artifactId" -< d
    scope      <- value "scope"      -< d
    groupId    <- value "groupId"    -< d
    returnA -< Dependency { .. }
  where
    addRuntimeScope = processTopDown (runtimeScope `when` noScope)
    value tag = getChildren >>> isElem >>> hasName tag
                >>> getChildren >>> getText
    noScope = isElem >>> hasName "dependency" >>>
              neg ( getChildren >>> isElem >>> hasName "scope" )
    runtimeScope = replaceChildren $ getChildren <+>
                   mkelem "scope" [] [txt "runtime"]

getRuntimeDeps :: [Dependency] -> [PluginName]
getRuntimeDeps deps = [ artifactId d | d <- deps,
                                       scope d == "runtime",
                                       groupId d == "org.jenkins-ci.plugins" ]


toNix :: [PluginInfo] -> String
toNix [] = "{}\n"
toNix l = "{\n" ++ show' (sort l) ++ "}\n"
  where
    show' = foldr ((++) . showPlugin) ""
    showPlugin (PluginInfo n v sha1 d) = 
      "  " ++ show n ++ " = {\n" ++
        "    version = " ++ show v ++ ";\n" ++
        "    sha1 = " ++ show sha1 ++ ";\n" ++
        "    depends = [ " ++ unwords (show <$> d) ++ " ];\n" ++
        "  };\n"


getPluginsInfo :: [PluginName] -> IO [PluginInfo]
getPluginsInfo [] = return []
getPluginsInfo names = getUnknown [] names where
  getUnknown _ [] = return []
  getUnknown seen names' = do
    new <- getInfo $ names' \\ (name <$> seen)
    more <- getUnknown (seen ++ new) (alldeps new)
    return $ new ++ more
  alldeps new = toList . fromList $ concat (depends <$> new)
  getInfo nn = sequence $ getLastest <$> nn
  getLastest name = head <$> getPluginVersions name >>= getPluginInfo name

main :: IO ()
main = getArgs >>= getPluginsInfo >>= putStr . toNix

