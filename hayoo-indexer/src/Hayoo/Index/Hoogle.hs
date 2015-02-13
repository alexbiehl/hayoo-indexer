module Hayoo.Index.Hoogle (
    Decl
  , Inst
  , MkURI
  , mkHaddockUri
  , indexHoogleArchive
  ) where

import Hayoo.Index.Hoogle.FunctionInfo
import Hayoo.Index.Hoogle.Parser
import Hayoo.Index.Hoogle.Conduit

mkHaddockUri :: String -> MkURI (Inst Decl)
mkHaddockUri base package version module_ decl anchor =
  base ++ "/package/" ++ package ++ "-"
   ++ version ++ "/docs/" ++ module' ++ ".html" ++ "#" ++ escape (anchor decl)
  where
    escape = concatMap (\c -> case c of
                               '\'' -> "-39-"
                               _    -> return c)

    module' = fmap (\c -> case c of
                           '.' -> '-'
                           _   -> c) module_
