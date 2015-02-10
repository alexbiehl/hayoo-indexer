{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import Control.Monad.IO.Class
import Hayoo.Index.Conduit
import Hayoo.Index.FunctionInfo
import Hayoo.Index.Hoogle
import Hayoo.Index.IndexSchema
import Hunt.Server.Client

class X a where x :: a

haddockUri :: String -> MkURI (Inst Decl)
haddockUri base package version module_ decl anchor =
  base ++ "/package/" ++ package ++ "-"
   ++ version ++ "/docs/" ++ module' ++ ".html" ++ "#" ++ escape (anchor decl)
  where
    escape = concatMap (\c -> case c of
                               '\'' -> "-39-"
                               _    -> return c)

    module' = fmap (\c -> case c of
                           '.' -> '-'
                           _   -> c) module_

main :: IO ()
main = do
  withHuntServer "http://localhost:3000" $ do
    r0 <- postCommand dropHayooIndexSchema
    liftIO $ putStrLn (show (r0 :: String))
    r1 <- postCommand createHayooIndexSchema
    liftIO $ putStrLn (show (r1 :: String))

    indexHoogleArchive (haddockUri "http://hackage.haskell.org") "hoogle.tar.gz"
