{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Logging via 'MonadLog'.
module Effectful.Log
  ( -- * Effect
    Log (..)

    -- ** Handlers
  , runLog
  , runNoLog

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
--
-- /Note:/ logging can be skipped by passing 'mempty' as the 'Logger', which
-- discards all messages while still requiring 'IOE'. Use 'runNoLog' instead
-- if 'IOE' is not otherwise present in the type signature.
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

-- | Run the 'Log' effect by discarding all messages.
--
-- This is useful for skipping the 'Log' effect when it is present in the
-- type signature but no actual logging back-end is available, e.g. when
-- running pure code with 'runPureEff'.
runNoLog :: Eff (Log : es) a -> Eff es a
runNoLog = reinterpret reader $ \env -> \case
  LogMessageOp {} -> pure ()
  LocalData data_ action -> localSeqUnlift env $ \unlift -> do
    (`local` unlift action) $ \logEnv -> logEnv { leData = data_ ++ leData logEnv }
  LocalDomain domain action -> localSeqUnlift env $ \unlift -> do
    (`local` unlift action) $ \logEnv -> logEnv { leDomain = leDomain logEnv ++ [domain] }
  LocalMaxLogLevel level action -> localSeqUnlift env $ \unlift -> do
    (`local` unlift action) $ \logEnv -> logEnv { leMaxLogLevel = level }
  GetLoggerEnv -> ask
  where
    reader = runReader LoggerEnv
      { leLogger = mempty
      , leComponent = mempty
      , leDomain = []
      , leData = []
      , leMaxLogLevel = defaultLogLevel
      }

-- | Orphan, canonical instance.
instance Log :> es => MonadLog (Eff es) where
  logMessage level message data_ = send $ LogMessageOp level message data_
  localData data_ action = send $ LocalData data_ action
  localDomain domain action = send $ LocalDomain domain action
  localMaxLogLevel level action = send $ LocalMaxLogLevel level action
  getLoggerEnv = send GetLoggerEnv
