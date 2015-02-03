{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Parser(
    Version
  , PackageName
  , Error
  , Comment
  , Synopsis
  , Decl(..)
  , Inst(..)
  , parseFile
  , parse
  ) where

import System.IO
import qualified Data.List as List

type Version = String

type PackageName = String

type Synopsis = String

data Decl = DeclEmpty
          | DeclModule !String
          | DeclData !Bool !String
          | DeclClass !String
          | DeclInstance !String
          | DeclType !String
          | DeclVersion !String
          | DeclPackage !String
          | DeclUnknown !String
          deriving (Show)

type Error = String

type Comment = String

data Inst a where
  InstComment :: String -> Inst a -> Inst a
  InstDecl    :: Decl -> Inst Decl

deriving instance Show (Inst a)

parseFile :: FilePath -> IO (PackageName, Version, Synopsis, [Inst Decl], [Error])
parseFile file = do
  withFile file ReadMode $ \handle -> do
    content <- hGetContents handle
    return $! parse content

parse :: String -> (PackageName, Version, Synopsis, [Inst Decl], [Error])
parse = parse' . List.lines

parse' :: [String] -> (PackageName, Version, Synopsis, [Inst Decl], [Error])
parse' = go "" "" "" [] []
  where
    go !syn !pname !ver !insts !errors [] =
      (pname, ver, syn, insts, errors)
    go !syn !pname !ver !insts !errors ls =
      withComment ls
      (\c r -> parseDecl r (goDecl (InstComment c . InstDecl)) goErr)
      (\r   -> parseDecl r (goDecl InstDecl) goErr)
      where
        goDecl wrap decl rest =
          go
          (synopsis syn  (wrap decl))
          (package pname (wrap decl))
          (version ver   (wrap decl))
          ((wrap decl):insts)
          errors
          rest
        goErr  err rest       =
          go syn pname ver insts (err:errors) rest

synopsis :: Synopsis -> Inst Decl -> Synopsis
synopsis _   (InstComment syn (InstDecl (DeclPackage _))) = syn
synopsis def _                                            = def

version :: Version -> Inst Decl -> Version
version def (InstComment _ d)          = version def d
version _   (InstDecl (DeclVersion v)) = v
version def (InstDecl _)               = def

package :: PackageName -> Inst Decl -> String
package def (InstComment _ d)          = package def d
package _   (InstDecl (DeclPackage p)) = p
package def (InstDecl _)               = def

withComment :: [String] -> (Comment -> [String] -> r) -> ([String] -> r) -> r
withComment ls k0 k1 = go0 ls
  where
    go0 (l:lx) =
      case l of
       ""                   -> go0 lx
       _ | isCommentStart l -> go id (l:lx)
         | isComment      l -> go0 lx
         | otherwise        -> k1 (l:lx)

    go acc (l:lx) =
      case l of
       ""                   -> go id lx
       _ | isComment      l -> go (acc . newline . comment) lx
         | otherwise        -> k0 (acc "") (l:lx)
      where
        comment = (List.drop 5 l ++)
        newline = ("\n" ++)

    isCommentStart l = List.isPrefixOf "-- | " l
    isComment      l = List.isPrefixOf "-- " l

parseDecl :: [String]
          -> (Decl -> [String] -> r)
          -> (Error -> [String] -> r)
          -> r
parseDecl ls k ke = go ls
  where
    go (l:lx) =
      case l of
       "" -> k DeclEmpty lx
       _ | startsWith "module "    -> k (DeclModule (List.drop 7 l)) lx
         | startsWith "type "      -> k (DeclType (List.drop 5 l)) lx
         | startsWith "data "      -> k (DeclData False (List.drop 5 l)) lx
         | startsWith "newtype "   -> k (DeclData True (List.drop 7 l)) lx
         | startsWith "class "     -> k (DeclClass (List.drop 5 l)) lx
         | startsWith "instance "  -> k (DeclInstance (List.drop 8 l)) lx
         | startsWith "@version "  -> k (DeclVersion (List.drop 9 l)) lx
         | startsWith "@package "  -> k (DeclPackage (List.drop 9 l)) lx
         | otherwise               -> parseTypeSig (l:lx) k ke
      where
        startsWith p = p `List.isPrefixOf` l

parseTypeSig :: [String]
             -> (Decl -> [String] -> r)
             -> (Error -> [String] -> r)
             -> r
parseTypeSig (l:lx) k ke = ke "Fuck" lx
