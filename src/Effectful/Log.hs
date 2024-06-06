{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Logging via 'MonadLog'.
module Effectful.Log
  ( -- * Effect
    Log (..)

    -- ** Handlers
  , runLog

    -- * Re-exports
  , module Log
  ) where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Time.Clock
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful
import Log

-- | Provide the ability to log messages via 'MonadLog'.
data Log :: Effect where
  LogMessageOp :: LogLevel -> Text -> Value -> Log m ()
  LocalData :: [Pair] -> m a -> Log m a
  LocalDomain :: Text -> m a -> Log m a
  LocalMaxLogLevel :: LogLevel -> m a -> Log m a
  GetLoggerEnv :: Log m LoggerEnv

type instance DispatchOf Log = Dynamic

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
runLog component logger maxLogLevel = reinterpret reader $ \env -> \case
  LogMessageOp level message data_ -> do
    time <- liftIO getCurrentTime
    logEnv <- ask
    liftIO $ logMessageIO logEnv time level message data_
  LocalData data_ action -> localSeqUnlift env $ \unlift -> do
    (`local` unlift action) $ \logEnv -> logEnv { leData = data_ ++ leData logEnv }
  LocalDomain domain action -> localSeqUnlift env $ \unlift -> do
    (`local` unlift action) $ \logEnv -> logEnv { leDomain = leDomain logEnv ++ [domain] }
  LocalMaxLogLevel level action -> localSeqUnlift env $ \unlift -> do
    (`local` unlift action) $ \logEnv -> logEnv { leMaxLogLevel = level }
  GetLoggerEnv -> ask
  where
    reader = runReader LoggerEnv
      { leLogger = logger
      , leComponent = component
      , leDomain = []
      , leData = []
      , leMaxLogLevel = maxLogLevel
      }

-- | Orphan, canonical instance.
instance Log :> es => MonadLog (Eff es) where
  logMessage level message data_ = send $ LogMessageOp level message data_
  localData data_ action = send $ LocalData data_ action
  localDomain domain action = send $ LocalDomain domain action
  localMaxLogLevel level action = send $ LocalMaxLogLevel level action
  getLoggerEnv = send GetLoggerEnv
