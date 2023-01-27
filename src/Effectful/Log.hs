{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Logging via 'MonadLog'.
module Effectful.Log
  ( -- * Effect
    Log

    -- ** Handlers
  , runLog
  , runLogWithEnv

    -- * Re-exports
  , module Log
  ) where

import Data.Text (Text)
import Data.Time.Clock
import Effectful.Dispatch.Static
import Effectful
import Log

-- | Provide the ability to log messages via 'MonadLog'.
data Log :: Effect

type instance DispatchOf Log = Static WithSideEffects
newtype instance StaticRep Log = Log LoggerEnv

-- | Run the 'Log' effect.
--
-- /Note:/ this is the @effectful@ version of 'runLogT'.
runLog
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> Logger
  -- ^ The logging back-end to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Log : es) a
  -- ^ The computation to run.
  -> Eff es a
runLog component logger maxLogLevel = evalStaticRep $ Log LoggerEnv
  { leLogger = logger
  , leComponent = component
  , leDomain = []
  , leData = []
  , leMaxLogLevel = maxLogLevel
  }

-- | Run the 'Log' effect with full logging options
runLogWithEnv
  :: IOE :> es
  => LoggerEnv
  -> Eff (Log : es) a
  -- ^ The computation to run.
  -> Eff es a
runLogWithEnv = evalStaticRep . Log

-- | Orphan, canonical instance.
instance Log :> es => MonadLog (Eff es) where
  logMessage level message data_ = do
    time <- unsafeEff_ getCurrentTime
    Log logEnv <- getStaticRep
    unsafeEff_ $ logMessageIO logEnv time level message data_

  localData data_ = localStaticRep $ \(Log logEnv) ->
    Log logEnv { leData = data_ ++ leData logEnv }

  localDomain domain = localStaticRep $ \(Log logEnv) ->
    Log logEnv { leDomain = leDomain logEnv ++ [domain] }

  localMaxLogLevel level = localStaticRep $ \(Log logEnv) ->
    Log logEnv { leMaxLogLevel = level }

  getLoggerEnv = do
    Log env <- getStaticRep
    pure env
