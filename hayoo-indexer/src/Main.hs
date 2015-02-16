{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hayoo.Index.Hoogle
import Hayoo.Index.Cabal

import Hayoo.Index.IndexSchema
import Hunt.Server.Client
import Control.Concurrent.Async

main :: IO ()
main = do

  sam <- newServerAndManager "http://localhost:3000"

  let runHunt = withServerAndManager sam

  let processHoogleArchive =
        indexHoogleArchive (mkHaddockUri "http://hackage.haskell.org") "hoogle.tar.gz"

  let processCabalArchive =
        indexCabalArchive (mkHackageUri "http://hackage.haskell.org") "index.tar.gz"

  runHunt $ do
    _ :: String <- postCommand dropHayooIndexSchema
    _ :: String <- postCommand createHayooIndexSchema
    return ()

  _ <- concurrently
       (runHunt processCabalArchive)
       (runHunt processHoogleArchive)
  return ()
