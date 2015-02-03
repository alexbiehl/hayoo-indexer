{-# LANGUAGE GADTs #-}
module FunctionInfo where

import Parser

data Ctx = CtxModule String
         | CtxClass  String
         | CtxModuleClass String String

data FunctionInfo = FunctionInfo {
    fiDescription :: !String
  , fiModule      :: !String
  , fiName        :: !String
  , fiPackage     :: !String
  , fiSignature   :: !String
  , fiSubsigs     :: !String
  , fiType        :: !String
  , fiVersion     :: !String
  , fiURI         :: !String
  } deriving (Show)


type Anchor = String

type MkURI = PackageName -> Version -> Decl -> String

mkEmptyUri :: MkURI
mkEmptyUri _ _ _ = ""

functionInfo :: PackageName -> Version -> [Inst Decl] -> [FunctionInfo]
functionInfo = concatMapDecls (toFunctionInfo mkEmptyUri)

toFunctionInfo :: MkURI
               -> Ctx
               -> PackageName
               -> Version
               -> Inst Decl
               -> [FunctionInfo]
toFunctionInfo mkUri ctx packageName version d = return
  FunctionInfo {
      fiURI         = mkUri packageName version (decl d)
    , fiDescription = description d
    , fiModule      = module_ ctx
    , fiName        = name (decl d)
    , fiPackage     = packageName
    , fiVersion     = version
    , fiSignature   = ""
    , fiSubsigs     = ""
    , fiType        = type_ ctx d
    }
  where
    decl :: Inst Decl -> Decl
    decl (InstComment _ x) = decl x
    decl (InstDecl x)      = x

    description :: Inst a -> String
    description (InstComment comment _) = comment
    description _                       = ""

    module_ (CtxModule moduleName) = moduleName
    module_ (CtxClass _) = ""
    module_ (CtxModuleClass moduleName _) = moduleName

    name (DeclModule moduleName) = moduleName
    name (DeclData _ dataName)   = dataName
    name (DeclClass className)   = className
    name (DeclType typeName )    = typeName
    name _                       = "unknown name"

    type_ ctx _ = "unknown"


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
       _         ->
         go1 moduleName className dx ++ f ctx packageName version d
      where
        ctx = CtxModuleClass moduleName className

    decl :: Inst Decl -> Decl
    decl (InstComment _ d) = decl d
    decl (InstDecl d)      = d
