{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Hayoo.Index.FunctionInfo
import Hayoo.Index.IndexSchema
import Hayoo.Index.Parser
import Hunt.ClientInterface
import Hunt.Server.Client

main :: IO ()
main = do

  (package, version, _, insts, _) <- parseFile "aeson.txt"

  let fi    = functionInfo package version insts
      !docs = fmap toApiDocument fi

  withHuntServer "http://localhost:3000" $ do
    r0 <- postCommand dropHayooIndexSchema
    liftIO $ putStrLn (show (r0 :: String))
    r1 <- postCommand createHayooIndexSchema
    liftIO $ putStrLn (show (r1 :: String))
    r2 <- postInsert docs
    liftIO $ putStrLn (show (r2))
