{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import Effectful
import Effectful.Time
import Test.Tasty
import Test.Tasty.HUnit
import Log

import Effectful.Log
import Effectful.Log.Backend.LogList

import StdoutExample ()
import qualified Utils as U

default (Text)

main :: IO ()
main = defaultMain $ testGroup "log-effectful"
    [ testCase "logMessage" testLogMessage
    , testCase "localData" testLocalData
    , testCase "localDomain" testLocalDomain
    , testCase "localMaxLogLevel" testLocalMaxLogLevel
    ]


testLogMessage :: Assertion
testLogMessage = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    logMessage LogTrace "Message 2" Null
    logMessage LogInfo "Message 3" Null
  U.assertEqual "" [message1 , message3] msgs


testLocalData :: Assertion
testLocalData = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    localData [ "key" .= "value" ] $ do
      logMessage LogInfo "Message 2" Null
    logMessage LogInfo "Message 3" Null
  U.assertEqual  ""
    [ message1
    , message2
      { lmData = object
        [ "__data_null" .= Null
        , "key" .= "value"
        ]
      }
    , message3
    ] msgs

testLocalDomain :: Assertion
testLocalDomain = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    localDomain "local domain" $ do
      logMessage LogInfo "Message 2" Null
    logMessage LogInfo "Message 3" Null
  U.assertEqual  ""
    [ message1
    , message2
      { lmDomain =
        [ "local domain"
        ]
      }
    , message3
    ] msgs

testLocalMaxLogLevel :: Assertion
testLocalMaxLogLevel = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    localMaxLogLevel LogAttention $ do
      logMessage LogInfo "Message 2" Null
    logMessage LogInfo "Message 3" Null
  U.assertEqual  ""
    [ message1
    , message3
    ] msgs

----------------------------------------
-- Helpers

logTestWith
  :: IOE :> es
  => Text
  -> LogLevel
  -> Eff (Logging : Time : es) ()
  -> Eff es [LogMessage]
logTestWith component logLevel k = do
  ll <- newLogList
  withLogListLogger ll $ \logger -> do
    runCurrentTimePure epoch . runLogging component logger logLevel $ k
  getLogList ll

epoch :: UTCTime
epoch = read "1970-01-01 00:00:00 UTC"

message :: LogMessage
message = LogMessage
  { lmComponent = "main"
  , lmDomain = []
  , lmTime = epoch
  , lmLevel = LogInfo
  , lmMessage = "Message"
  , lmData = object [ "__data_null" .= Null ]
  }

message1 :: LogMessage
message1 = message
  { lmMessage = "Message 1"
  }

message2 :: LogMessage
message2 = message
  { lmMessage = "Message 2"
  }

message3 :: LogMessage
message3 = message
  { lmMessage = "Message 3"
  }
