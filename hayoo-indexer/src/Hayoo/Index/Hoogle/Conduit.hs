{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Hayoo.Index.Hoogle.Conduit where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Conduit
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.String.UTF8 as UTF8
import           Hayoo.Index.Hoogle.FunctionInfo
import           Hayoo.Index.Hoogle.Parser
import           Hunt.ClientInterface
import           Hunt.Conduit
import           Hunt.Server.Client
import           System.FilePath

type Error = String

indexHoogleArchive :: MkURI (Inst Decl)
                   -> FilePath
                   -> HuntConnectionT IO ()
indexHoogleArchive mkUri fp = do
  archive <- liftIO (ByteString.readFile fp)
  sourceArchive (GZip.decompress archive)
    =$= functionInfos mkUri -- | Parse hoogle databases
    =$= tracer              -- | Print errors
    =$= makeInserts toApiDocument
    =$= rechunkCommands 100
    $$ cmdSink

sourceArchive :: Monad m
              => ByteString
              -> Producer m (FilePath, ByteString)
sourceArchive = unfoldC go . Tar.read
  where
    go (Tar.Next entry next) =
      case Tar.entryContent entry of
        Tar.NormalFile content _ -> do
          Just ((Tar.entryPath entry, content), next)
        _                        -> go next
    go Tar.Done     = Nothing
    go (Tar.Fail _) = Nothing

functionInfos :: Monad m
               => MkURI (Inst Decl)
               -> ConduitM (FilePath, ByteString) (Either Error FunctionInfo) m ()
functionInfos mkUri = awaitForever $ \(fp, content) -> do
  let
      (package:version:_) = splitDirectories fp
      (insts, errors)     = parseHoogle (utf8 content)
      infos               = functionInfo mkUri package version insts
      dedupe              = List.nubBy ((==) `on` fiURI) infos
      dispError err       = package ++ "-" ++ version ++ ": " ++ err
  yieldMany (fmap (Left . dispError) errors)
  yieldMany (fmap Right dedupe)
  where
    utf8 :: ByteString -> String
    utf8 = UTF8.toString . UTF8.fromRep

tracer :: (MonadIO m) => ConduitM (Either Error a) a m ()
tracer = awaitForever $ \a -> do
  case a of
    Left err -> liftIO $ putStrLn err
    Right x  -> yield x
