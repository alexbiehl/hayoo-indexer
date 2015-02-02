{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Parser where


import System.IO
import Data.Monoid
import qualified Data.List as List

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

parseFile :: FilePath -> IO ([Inst Decl], [Error])
parseFile file = do
  withFile file ReadMode $ \handle -> do
    content <- hGetContents handle
    return $! parse content

parse :: String -> ([Inst Decl], [Error])
parse = parse' . List.lines

parse' :: [String] -> ([Inst Decl], [Error])
parse' = go [] []
  where
    go !insts !errors [] = (insts, errors)
    go !insts !errors ls =
      withComment ls
      (\c r -> parseDecl r (goDecl (InstComment c . InstDecl)) goErr)
      (\r   -> parseDecl r (goDecl InstDecl) goErr)
      where
        goDecl wrap decl rest = go (insert (wrap decl) insts) errors rest
        goErr  err rest       = go insts (err:errors) rest

insert :: Inst Decl -> [Inst Decl] -> [Inst Decl]
insert decl decls = decl:decls

withComment :: [String] -> (Comment -> [String] -> r) -> ([String] -> r) -> r
withComment ls k0 k1 = go0 ls
  where
    go0 (l:lx) =
      case l of
       ""                   -> go0 lx
       _ | isCommentStart l -> go id (l:lx)
         | isComment      l -> go id (l:lx)
         | otherwise        -> k1 (l:lx)

    go acc (l:lx) =
      case l of
       ""                   -> go id lx
       _ | isCommentStart l -> go (acc . newline . comment) lx
         | isComment      l -> go (acc . newline . comment) lx
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
