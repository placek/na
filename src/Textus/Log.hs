module Textus.Log where

import           Data.Text
import           Polysemy

data Log m a where
  LogInfo :: Text -> Log m ()
  LogWarn :: Text -> Log m ()
  LogDebug :: Text -> Log m ()
  LogError :: Text -> Log m ()
  LogFatal :: Text -> Log m ()

makeSem ''Log

interpretLog :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
interpretLog = interpret \case
  LogInfo  msg -> embed . putStrLn . unpack $ "[INFO] " <> msg
  LogWarn  msg -> embed . putStrLn . unpack $ "[WARN] " <> msg
  LogDebug msg -> embed . putStrLn . unpack $ "[DEBUG] " <> msg
  LogError msg -> embed . putStrLn . unpack $ "[ERROR] " <> msg
  LogFatal msg -> embed . putStrLn . unpack $ "[FATAL] " <> msg
