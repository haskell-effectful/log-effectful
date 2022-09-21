{-# LANGUAGE OverloadedStrings #-}
module Main where

import Effectful
import Log
import Log.Backend.StandardOutput

import Effectful.Log

main :: IO ()
main = runEff $ do
  withStdOutLogger $ \logger -> do
    runLog "main" logger defaultLogLevel app

app :: Log :> es => Eff es ()
app = localData ["local_char" .= 'x'] $ do
  logInfo "Hello!" $ object
    [ "payload" .= (123::Int)
    ]
