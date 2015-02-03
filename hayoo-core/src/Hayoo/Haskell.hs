module Hayoo.Haskell where

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension

pphsMode :: PPHsMode
pphsMode = PPHsMode {
    classIndent   = 0
  , doIndent      = 0
  , multiIfIndent = 0
  , caseIndent    = 0
  , letIndent     = 0
  , whereIndent   = 0
  , onsideIndent  = 0
  , spacing       = False
  , layout        = PPNoLayout
  , linePragmas   = False
  }

pretty :: Pretty a => a -> String
pretty = prettyPrintWithMode pphsMode

parseMode :: ParseMode
parseMode =
  defaultParseMode {
    extensions = parseModeExtensions
    }

parseModeExtensions :: [Extension]
parseModeExtensions =
  fmap EnableExtension [
      ConstraintKinds
    , EmptyDataDecls
    , TypeOperators
    , ExplicitForAll
    , GADTs
    , KindSignatures
    , MultiParamTypeClasses
    , TypeFamilies
    , FlexibleContexts
    , FunctionalDependencies
    , ImplicitParams
    , MagicHash
    , UnboxedTuples
    , ParallelArrays
    , UnicodeSyntax
    , DataKinds
    , PolyKinds
    ]

parse :: Parseable a => String -> Either String a
parse s =
  case parseWithMode parseMode s of
   ParseOk a         -> return a
   ParseFailed _ err -> Left err
