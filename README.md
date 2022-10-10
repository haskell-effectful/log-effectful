# log-effectful

[![Build Status](https://github.com/haskell-effectful/log-effectful/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/haskell-effectful/log-effectful/actions?query=branch%3Amaster)
[![Hackage](https://img.shields.io/hackage/v/log-effectful.svg)](https://hackage.haskell.org/package/log-effectful)
[![Dependencies](https://img.shields.io/hackage-deps/v/log-effectful.svg)](https://packdeps.haskellers.com/feed?needle=andrzej@rybczak.net)
[![Stackage LTS](https://www.stackage.org/package/log-effectful/badge/lts)](https://www.stackage.org/lts/package/log-effectful)
[![Stackage Nightly](https://www.stackage.org/package/log-effectful/badge/nightly)](https://www.stackage.org/nightly/package/log-effectful)

Adaptation of the [log-base](https://hackage.haskell.org/package/log-base)
library for the effectful ecosystem.

## Example

A sample usage for logging to both standard output and Elasticsearch:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Effectful
import Effectful.Log
import Log.Backend.ElasticSearch
import Log.Backend.StandardOutput

main :: IO ()
main = runEff $ do
  let config = defaultElasticSearchConfig
        { esServer = "http://localhost:9200"
        , esIndex  = "logs"
        }
  withStdOutLogger $ \stdoutLogger -> do
    withElasticSearchLogger config $ \esLogger -> do
      runLog "main" (stdoutLogger <> esLogger) defaultLogLevel $ do
        logInfo_ "Hi there"
```
