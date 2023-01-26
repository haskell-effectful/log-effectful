{-# LANGUAGE OverloadedStrings #-}
module Main where

import Effectful
import Effectful.Log
import Log.Backend.StandardOutput

main :: IO ()
main = runEff $ do
  withStdOutLogger $ \logger -> do
    runLog "main" logger defaultLogLevel [] [] app

app :: Log :> es => Eff es ()
app = localData ["local_char" .= 'x'] $ do
  logInfo "Hello!" $ object
    [ "payload" .= (123::Int)
    ]
