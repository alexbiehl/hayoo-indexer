module Hayoo.Index.Cabal where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import           Hayoo.Index.Cabal.PackageInfo
import           Hayoo.Index.Conduit
import           Hunt.ClientInterface
import           Hunt.Conduit
import           Hunt.Server.Client
import Conduit

indexCabalArchive :: FilePath -> HuntConnectionT IO ()
indexCabalArchive fp = do
  archive <- liftIO $ ByteString.readFile fp
  compressedArchive archive
  =$= packageInfos
  =$= leftLogger "indexCabalArchive: "
  =$= makeInserts toApiDocument
  =$= rechunkCommands 50
  $$ cmdSink

packageInfos :: Monad m => Conduit (FilePath, ByteString) m (Either Error PackageDescription)
packageInfos = mapC (parseCabalFile . utf8 . snd)
