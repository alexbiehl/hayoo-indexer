module Hayoo.Query where

import qualified Hayoo.Signature as Signature

import           Control.Monad
import qualified Data.List as List
import qualified Data.Set as Set
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

    queries = [
        parseHuntQuery s
      , parseSignatureQuery s
      , parseDefaultQuery s
      ]

parseHuntQuery :: String -> [Hunt.Query]
parseHuntQuery s =
  case Hunt.parseQuery s of
   Left _  -> mzero
   Right q -> return q

parseSignatureQuery :: String -> [Hunt.Query]
parseSignatureQuery s =
  Hunt.qOrs (List.concat [sigQuery, subSigQuery])
  where
    sig = case Signature.parseNormalized s of
      Left _  -> mzero
      Right a -> return a

    subSigs = List.concatMap (
      Set.toList
      . Signature.explodeNormalized
      ) sig

    subSigQuery =
      return
      . setBoost 0.1
      . setContexts ["subsig"]
      . qAnds (fmap (
                  qFullWord
                  . Signature.pretty
                  ) subSigs)

    sigQuery = fmap (
      setContext ["signature"]
      . qWord
      . pretty
      ) sig

parseDefaultQuery :: String -> [Hunt.Query]
parseDefaultQuery =
  return . Hunt.qAnds . fmap qWordNoCase . List.words
