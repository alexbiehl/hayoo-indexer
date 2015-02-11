{-# LANGUAGE GADTs #-}
module Hayoo.Index.Hoogle.FunctionInfo where

import           Control.Monad
import           Data.Aeson (toJSON)
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Hayoo.Signature as Signature
import qualified Hunt.ClientInterface as Hunt
import qualified Hunt.Common.DocDesc as Hunt
import           Hayoo.Index.IndexSchema
import           Hayoo.Index.Hoogle.Parser

type PackageName = String
type Version = String
type ModuleName = String
type Anchor = String

data FunctionInfo = FunctionInfo {
    fiDescription :: !Text
  , fiModule      :: !Text
  , fiName        :: !Text
  , fiPackage     :: !Text
  , fiSignature   :: !Text
  , fiSubsigs     :: !Text
  , fiType        :: !Text
  , fiVersion     :: !Text
  , fiURI         :: !Text
  } deriving (Show)

instance Hunt.Huntable FunctionInfo where
  huntURI      = fiURI
  huntDescr    = Hunt.mkDocDesc
                 . fmap toJSON
                 . HashMap.fromList
                 . toList
  huntIndexMap = Map.fromList . toList

type MkURI a = PackageName -> Version -> ModuleName -> a -> (a -> Anchor) -> String

toList :: FunctionInfo -> [(Text, Text)]
toList fi = filter (not . Text.null . snd) [
    c'description * fiDescription fi
  , c'module      * fiModule fi
  , c'name        * fiName fi
  , c'package     * fiPackage fi
  , c'signature   * fiSignature fi
  , c'subsig      * fiSubsigs fi
  , c'type        * fiType fi
  , c'version     * fiVersion fi
  ]
  where
    (*) = (,)

functionInfo :: MkURI (Inst Decl) -> PackageName -> Version -> [Inst Decl] -> [FunctionInfo]
functionInfo mkUri package version = concatMap (toFunctionInfo mkUri package version)

toFunctionInfo :: MkURI (Inst Decl)
               -> PackageName
               -> Version
               -> Inst Decl
               -> [FunctionInfo]
toFunctionInfo mkUri packageName version d =
  return FunctionInfo {
      fiURI         = Text.pack $ mkUri packageName version (declModule d) d haddockAnchor
    , fiDescription = Text.pack $ declDescription d
    , fiModule      = Text.pack $ declModule d
    , fiName        = Text.pack $ declName d
    , fiPackage     = Text.pack $ packageName
    , fiVersion     = Text.pack $ version
    , fiSignature   = Text.pack $ signature
    , fiSubsigs     = Text.pack $ subsignatures
    , fiType        = Text.pack $ declType d
    }
  where
    signature = maybe "" Signature.pretty $ do
      sig <- declSignature d
      return (Signature.normalize sig)

    subsignatures =
      List.intercalate "\n" (mkSubsignatures signature)

mkSubsignatures :: String -> [String]
mkSubsignatures s = do
  signature    <- either (const mzero) return (Signature.parse s)
  subsignature <- Foldable.toList (Signature.explode signature)
  return (Signature.pretty subsignature)
