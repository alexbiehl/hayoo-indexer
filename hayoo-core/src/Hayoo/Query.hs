{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Query where

import qualified Hayoo.Signature as Signature

import           Control.Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Hunt.Query.Language.Builder as Hunt
import qualified Hunt.Query.Language.Grammar as Hunt
import qualified Hunt.Query.Language.Parser as Hunt

parse :: String -> Hunt.Query
parse s = Hunt.qOrs (List.concat queries)
  where
    huntQuery      = parseHuntQuery s
    signatureQuery = parseSignatureQuery s

    queries = [
        huntQuery
      , signatureQuery
      , if List.null huntQuery && List.null signatureQuery
        then parseDefaultQuery s
        else mzero
      ]

parseHuntQuery :: String -> [Hunt.Query]
parseHuntQuery = Foldable.toList . Hunt.parseQuery

parseSignatureQuery :: String -> [Hunt.Query]
parseSignatureQuery s =
  [ Hunt.qOrs (List.concat [sigQuery, subSigQuery]) ]
  where
    sig = Foldable.toList (Signature.parseNormalized s)

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

parseDefaultQuery :: String -> [Hunt.Query]
parseDefaultQuery =
  return . Hunt.qAnds . fmap Hunt.qWordNoCase . fmap Text.pack  . List.words
