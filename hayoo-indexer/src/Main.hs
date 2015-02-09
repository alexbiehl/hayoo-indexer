{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.String.UTF8 as UTF8
import           Hayoo.Index.FunctionInfo
import           Hayoo.Index.Hoogle
import           Hayoo.Index.IndexSchema
import           Hunt.ClientInterface
import           Hunt.Server.Client
import           System.FilePath

utf8 :: ByteString -> String
utf8 = UTF8.toString . UTF8.fromRep

readHoogleArchive :: MkURI (Inst Decl) -> ByteString -> [FunctionInfo]
readHoogleArchive mkUri = concat . go [] . Tar.read . GZip.decompress
  where
    go !acc (Tar.Next entry next) =
      case Tar.entryContent entry of
       Tar.NormalFile content _ ->
         let
           (package:version:_) = splitDirectories (Tar.entryPath entry)
           (insts, _)          = parseHoogle (utf8 content)
           fis                 = functionInfo mkUri package version insts
           dedupe              = List.nubBy ((==) `on` fiURI) fis
         in
          go (dedupe:acc) next
       _ -> go acc next
    go !acc Tar.Done              = acc
    go !acc (Tar.Fail _)          = acc

haddockUri :: String -> MkURI (Inst Decl)
haddockUri base package version module_ decl anchor =
  base ++ "/package/" ++ package ++ "-"
   ++ version ++ "/docs/" ++ module' ++ ".html" ++ "#" ++ anchor decl
  where
    module' = fmap (\c -> case c of
                           '.' -> '-'
                           _   -> c) module_

main :: IO ()
main = do
  archive <- readHoogleArchive (haddockUri "https://hackage.haskell.org")
               <$> ByteString.readFile "hoogle.tar.gz"

  let post =
        postCommand . cmdSequence . fmap cmdInsertDoc . fmap toApiDocument

  withHuntServer "http://localhost:3000" $ do
    r0 <- postCommand dropHayooIndexSchema
    liftIO $ putStrLn (show (r0 :: String))
    r1 <- postCommand createHayooIndexSchema
    liftIO $ putStrLn (show (r1 :: String))

    forM_ (Split.chunksOf 150 archive) $
      (post :: [FunctionInfo] -> HuntConnectionT IO String)
