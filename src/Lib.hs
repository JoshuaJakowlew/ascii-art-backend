{-# LANGUAGE OverloadedStrings #-}

module Lib ( runApp
           ) where

import           Web.Spock
import           Web.Spock.Config
import           Config
import           Routes.AsciiArt

runApp :: IO ()
runApp = do
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyState
    runSpock 8080 (spock spockCfg app)

app :: Api
app = do
    get "asciiArt" $ asciiArt
