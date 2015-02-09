{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Hayoo.Index.Hoogle (
    parseHoogle
  , Inst
  , Decl
  , declDescription
  , declSignature
  , declName
  , declType
  , declModule
  , haddockAnchor
  )
       where

import           Data.Char
import           Data.Either (lefts, rights)
import qualified Data.List as List
import           Data.List.Split
import           Hayoo.Haskell
import qualified Hayoo.Haskell as Haskell
import           Hayoo.Signature
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

type Error = String

data Inst a where
  ICommented :: String -> Inst a -> Inst a
  IModule    :: String -> Inst a -> Inst a
  IDecl      :: Decl   -> Inst Decl

deriving instance Show (Inst a)

parseHoogle' :: String -> [Either String (Module, [Comment])]
parseHoogle' =  fmap parseWithComments .  fmap unlines . fixup . lines

parseHoogle :: String -> ([Inst Decl], [Error])
parseHoogle hoogle = (insts r, errs r)
  where
    r = parseHoogle' hoogle
    insts = concatMap (uncurry moduleInsts) . rights
    errs  = lefts

splitModules :: [String] -> [[String]]
splitModules = split (keepDelimsL (whenElt (List.isPrefixOf "module ")))

fixup :: [String] -> [[String]]
fixup = splitModules
        . List.filter (not . List.isPrefixOf "@")
        . fmap fixupModule
        . fmap fixupNewtype
        . fmap fixupDataDecl
        . fmap fixupInstance
        . fmap fixupComment
  where
    fixupComment s | List.isPrefixOf "-- | " s = "--  " ++ List.drop 4 s
                   | otherwise                 = s

    fixupModule s | List.isPrefixOf "module " s = s ++ " where"
                   | otherwise                  = s

    fixupNewtype s | List.isPrefixOf "newtype " s = "data" ++ List.drop 7 s
                   | otherwise                    = s

    fixupInstance s | List.isPrefixOf "instance " s = ""
                    | otherwise                     = s

    fixupDataDecl []                 = []
    fixupDataDecl (c:cx) | isUpper c = "data XX_Data where " ++ (c:cx)
                         | otherwise = (c:cx)

mergeComments :: [Comment] -> [Comment]
mergeComments []         = []
mergeComments (cmm1:cmm2:cx) | isConsecutive = mergeComments (comment':cx)
  where
    Comment _ span1 content1 = cmm1
    Comment _ span2 content2 = cmm2
    comment' = Comment
               False
               (mergeSrcSpan span1 span2)
               (content1 ++ "\n" ++ content2)
    isConsecutive =
      srcSpanEndLine span1 + 1 == srcSpanStartLine span2
mergeComments (c:cx)     = c : mergeComments cx

withComment :: Decl -> SrcLoc -> [Comment] -> Inst Decl
withComment decl _      []                              = IDecl decl
withComment decl srcLoc (Comment _ srcSpan comment:cx) =
  case srcLine srcLoc of
   i | i == (srcSpanEndLine srcSpan + 1) -> ICommented comment (IDecl decl)
     | srcSpanEndLine srcSpan > i        -> IDecl decl
     | otherwise                      -> withComment decl srcLoc cx

moduleInsts :: Module -> [Comment] -> [Inst Decl]
moduleInsts (Module _ name _ _ _ _ decls) =
  fmap (IModule (Haskell.pretty name)) . declInsts decls . mergeComments

declInsts :: [Decl] -> [Comment] -> [Inst Decl]
declInsts []     _        = []
declInsts (d:dx) comments =
  case d of
   TypeDecl srcLoc _ _ _          -> withComment d srcLoc comments : declInsts dx comments
   DataDecl srcLoc _ _ _ _ _ _    -> withComment d srcLoc comments : declInsts dx comments
   GDataDecl srcLoc _ _ _ _ _ _ _ -> withComment d srcLoc comments : declInsts dx comments
   ClassDecl srcLoc _ _ _ _ _     -> withComment d srcLoc comments : declInsts dx comments
   TypeSig srcLoc _ _             -> withComment d srcLoc comments : declInsts dx comments
   _                              -> declInsts dx comments

declDescription :: Inst Decl -> String
declDescription (ICommented comment _) = comment
declDescription (IModule _ decl)       = declDescription decl
declDescription _                      = ""

declSignature :: Inst Decl -> Maybe Signature
declSignature (ICommented _ decl) = declSignature decl
declSignature (IModule _ decl) = declSignature decl
declSignature (IDecl decl) =
  case decl of
   TypeDecl _ _ _ t                           -> return (fromType t)
   GDataDecl _ _ _ _ _ _ [GadtDecl _ _ _ t] _ -> return (fromType t)
   TypeSig _ _ t                              -> return (fromType t)
   _                                          -> Nothing

declName :: Inst Decl -> String
declName (ICommented _ decl) = declName decl
declName (IModule _ decl) = declName decl
declName (IDecl decl) =
  case decl of
   TypeSig _ [name] _                                     -> Haskell.pretty name
   TypeDecl _ name _ _                                    -> Haskell.pretty name
   GDataDecl _ _ _ (Ident "XX_Data") _ _ [GadtDecl _ name _ _] _ -> Haskell.pretty name
   GDataDecl _ _ _ name _ _ _ _                           -> Haskell.pretty name
   DataDecl _ _ _ name _ _ _                              -> Haskell.pretty name
   ClassDecl _ _ name _ _ _                               -> Haskell.pretty name
   _                                                      -> ""

declModule :: Inst Decl -> String
declModule (ICommented _ decl) = declModule decl
declModule (IModule moduleName _) = moduleName
declModule (IDecl _) = ""

declType :: Inst Decl -> String
declType (IModule _ decl) = declType decl
declType (ICommented _ decl) = declType decl
declType (IDecl decl) =
  case decl of
   TypeSig _ _ _                              -> "function"
   TypeDecl _ _ _ _                           -> "type"
   GDataDecl _ _ _ _ _ _ [GadtDecl _ _ _ _] _ -> "function"
   GDataDecl _ _ _ _ _ _ _ _                  -> "data"
   ClassDecl _ _ _ _ _ _                      -> "class"
   DataDecl _ _ _ _ _ _ _                     -> "data"
   _                                          -> "unknown"

haddockAnchor :: Inst Decl -> String
haddockAnchor (IModule _ decl)    = haddockAnchor decl
haddockAnchor (ICommented _ decl) = haddockAnchor decl
haddockAnchor (IDecl decl)        =
  case decl of
   TypeSig _ _ _                              -> "v:" ++ declName (IDecl decl)
   GDataDecl _ _ _ _ _ _ [GadtDecl _ _ _ _] _ -> "v:" ++ declName (IDecl decl)
   _                                          -> "t:" ++ declName (IDecl decl)
