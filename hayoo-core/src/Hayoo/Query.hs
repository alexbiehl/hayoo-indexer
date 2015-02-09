{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Query where

import qualified Hayoo.Signature as Signature

import           Control.Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Hunt.Query.Language.Builder as Hunt
import qualified Hunt.Query.Language.Grammar as Hunt
import qualified Hunt.Query.Language.Parser as Hunt

parse :: Text -> Hunt.Query
parse s = Hunt.qOrs (List.concat queries)
  where
    unquoted       = removeQuotes s

    huntQuery      = parseHuntQuery unquoted
    signatureQuery = parseSignatureQuery unquoted

    queries = [
        huntQuery
      , signatureQuery
      , parseDefaultQuery s
      ]

parseHuntQuery :: Text -> [Hunt.Query]
parseHuntQuery = Foldable.toList . Hunt.parseQuery . Text.unpack

parseSignatureQuery :: Text -> [Hunt.Query]
parseSignatureQuery s =
  if List.null queries
     then []
     else [ Hunt.qOrs queries ]
  where
    queries = List.concat [sigQuery, subSigQuery]

    parseSignature s | Text.isInfixOf "->" s =
                         Signature.parseNormalized (Text.unpack s)
                     | Text.isInfixOf "=>" s =
                         Signature.parseNormalized (Text.unpack s)
                     | otherwise            =
                         Left ""

    sig = Foldable.toList (parseSignature s)

    subSigs = List.concatMap (
      Foldable.toList
      . Signature.explodeNormalized
      ) sig

    subSigQuery =
      return
      . Hunt.setBoost 0.1
      . Hunt.setContexts ["subsig"]
      $ Hunt.qAnds (fmap (
                       Hunt.qFullWord
                       . pack
                       . Signature.pretty
                  ) subSigs)

    sigQuery = fmap (
      Hunt.setContexts ["signature"]
      . Hunt.qWord
      . pack
      . Signature.pretty
      ) sig

pack :: String -> Text.Text
pack = Text.pack

parseDefaultQuery :: Text -> [Hunt.Query]
parseDefaultQuery =
  return . Hunt.qAnds . fmap Hunt.qWordNoCase . fmap removeQuotes . Text.words

removeQuotes :: Text -> Text
removeQuotes t
  | Text.null t        = t
  | Text.head t == '"'
    &&
    Text.last t == '"' = Text.dropAround (== '"') t
  | Text.head t == '\''
    &&
    Text.last t == '\'' = Text.dropAround (== '\'') t
  | otherwise           = t
