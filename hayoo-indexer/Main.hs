{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import FunctionInfo
import Parser

import Control.Monad

import Hunt.IndexSchema

import Hunt.ClientInterface
import Hunt.Server.Client

import Control.Monad.IO.Class

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
