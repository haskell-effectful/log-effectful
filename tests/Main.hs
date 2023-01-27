{-# LANGUAGE OverloadedStrings #-}
module Main where

import Effectful
import Effectful.Log
import Log.Backend.StandardOutput
import qualified Data.Aeson.Types as Aeson

main :: IO ()
main = runEff $ do
  withStdOutLogger $ \logger -> do
    runLog "main" logger defaultLogLevel app
  withStdOutLogger $ \logger -> do
    let logEnv = LoggerEnv  { leLogger = logger
                            , leComponent = "main"
                            , leDomain = []
                            , leData = [("testKey", Aeson.String "testValue") ]
                            , leMaxLogLevel = defaultLogLevel
                            }
    runLogWithEnv logEnv app

app :: Log :> es => Eff es ()
app = localData ["local_char" .= 'x'] $ do
  logInfo "Hello!" $ object
    [ "payload" .= (123::Int)
    ]
