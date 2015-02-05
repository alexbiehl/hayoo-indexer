{-# LANGUAGE GADTs #-}
module Hayoo.Index.FunctionInfo where

import           Control.Arrow (second)
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
import           Hunt.IndexSchema
import           Parser

instance Hunt.Huntable FunctionInfo where
  huntURI      = fiURI
  huntDescr    = Hunt.mkDocDesc
                 . fmap toJSON
                 . HashMap.fromList
                 . toList
  huntIndexMap = Map.fromList . toList

data Ctx = CtxModule String
         | CtxClass  String
         | CtxModuleClass String String

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

type MkURI = PackageName -> Version -> String -> Decl -> String

toList :: FunctionInfo -> [(Text, Text)]
toList = fmap (second Text.pack) [
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

mkUri :: MkURI
mkUri p v m d = p ++ v ++ m ++ show d

mkEmptyUri :: MkURI
mkEmptyUri _ _ _ _ = ""

functionInfo :: PackageName -> Version -> [Inst Decl] -> [FunctionInfo]
functionInfo = concatMapDecls (toFunctionInfo mkUri)

toFunctionInfo :: MkURI
               -> Ctx
               -> PackageName
               -> Version
               -> Inst Decl
               -> [FunctionInfo]
toFunctionInfo mkUri ctx packageName version d =
  return FunctionInfo {
      fiURI         = Text.pack $ mkUri packageName version (module_ ctx)
    , fiDescription = Text.pack $ description d
    , fiModule      = Text.pack $ module_ ctx
    , fiName        = Text.pack $ name decl
    , fiPackage     = Text.pack $ packageName
    , fiVersion     = Text.pack $ version
    , fiSignature   = Text.pack $ signature decl
    , fiSubsigs     = Text.pack $ subsignatures decl
    , fiType        = Text.pack $ type_ ctx decl
    }
  where
    toDecl :: Inst Decl -> Decl
    toDecl (InstComment _ x) = decl x
    toDecl (InstDecl x)      = x

    decl = toDecl d

    description :: Inst a -> String
    description (InstComment comment _) = comment
    description _                       = ""

    module_ (CtxModule moduleName)        = moduleName
    module_ (CtxClass _)                  = ""
    module_ (CtxModuleClass moduleName _) = moduleName

    name (DeclModule moduleName) = moduleName
    name (DeclData _ dataName)   = dataName
    name (DeclClass className)   = className
    name (DeclType typeName _)   = typeName
    name (DeclTypeSig sigName _) = sigName
    name _                       = "unknown name"

    signature (DeclTypeSig _ sig) = sig
    signature (DeclType _ sig)    = sig
    signature _                   = ""

    subsignatures =
      List.intercalate "\n" . mkSubsignatures . signature

    type_ (CtxClass _) (DeclTypeSig _ _)         = "method"
    type_ (CtxModuleClass _ _) (DeclTypeSig _ _) = "method"
    type_ _   (DeclTypeSig _ _)                  = "function"
    type_ _   (DeclType _ _)                     = "type"
    type_ _   (DeclData False _)                 = "data"
    type_ _   (DeclData True  _)                 = "newtype"
    type_ _   (DeclModule _)                     = "module"
    type_ _   (DeclClass _)                      = "class"
    type_ _   _                                  = "unknown"

mkSubsignatures :: String -> [String]
mkSubsignatures s =
  signature    <- either mzero return (Signature.parse s)
  subsignature <- Foldable.toList (Signature.explode signature)
  return (Signature.pretty subsignature)

concatMapDecls :: (Ctx -> PackageName -> Version -> Inst Decl -> [FunctionInfo])
               -> PackageName
               -> Version
               -> [Inst Decl]
               -> [FunctionInfo]
concatMapDecls f packageName version = go
  where
    go :: [Inst Decl] -> [FunctionInfo]
    go []     = []
    go (d:dx) =
      case decl d of
       DeclModule moduleName ->
         go0 moduleName dx ++ f (CtxModule moduleName) packageName version d
       DeclClass className   ->
         go1 "" className dx
       _ -> go dx

    go0 _          []     = []
    go0 moduleName (d:dx) =
      case decl d of
       DeclModule moduleName' ->
         go0 moduleName' dx ++ f (CtxModule moduleName') packageName version d
       DeclClass className    ->
         go1 moduleName className dx ++ f ctx packageName version d
       _                      ->
         go0 moduleName dx ++ f ctx packageName version d
      where
        ctx = CtxModule moduleName

    go1 _          _         []     = []
    go1 moduleName className (d:dx) =
      case decl d of
       DeclEmpty -> go0 moduleName dx
       DeclModule moduleName' ->
         go0 moduleName' dx ++ f ctx packageName version d
       _         ->
         go1 moduleName className dx ++ f ctx packageName version d
      where
        ctx = CtxModuleClass moduleName className

    decl :: Inst Decl -> Decl
    decl (InstComment _ d) = decl d
    decl (InstDecl d)      = d
