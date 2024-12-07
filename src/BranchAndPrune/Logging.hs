{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BranchAndPrune.Logging (BPLogConfig (..), defaultBPLogConfig, getLoggingFunctions) where

import BranchAndPrune.BranchAndPrune (LoggingFunctions (..))
import BranchAndPrune.Steps (Step (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.Aeson qualified as A
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.IO (Handle, IOMode (WriteMode), hClose, hPutStrLn, openFile)

-- import Database.Redis qualified as Redis

data BPLogConfig = BPLogConfig
  { shouldLogDebugMessages :: Bool,
    stepsFile :: Maybe FilePath,
    stepsRedisStream :: Maybe String
  }
  deriving (Eq)

defaultBPLogConfig :: BPLogConfig
defaultBPLogConfig =
  BPLogConfig
    { shouldLogDebugMessages = False,
      stepsFile = Nothing,
      stepsRedisStream = Nothing
    }

data BPLogResources = LogResources
  { stepsFileHandle :: Maybe Handle
  -- redisConnection ::
  }

getLoggingFunctions ::
  (MonadIO m, MonadLogger m, A.ToJSON problem, A.ToJSON paving) =>
  BPLogConfig ->
  LoggingFunctions BPLogResources (Step problem paving) m
getLoggingFunctions logConfig =
  LoggingFunctions {..}
  where
    initLogging = do
      stepsFileHandle <- case logConfig.stepsFile of
        Just path -> liftIO $ do
          handle <- openFile path WriteMode
          hPutStrLn handle "["
          pure $ Just handle
        Nothing -> pure Nothing
      pure $ LogResources {..}

    finaliseLogging resources = do
      case resources.stepsFileHandle of
        Just handle -> liftIO $ do
          hPutStrLn handle "]"
          hClose handle
        _ -> pure ()

    logStep resources step = do
      let stepLine = (commaIfNotInit step) <> (bsToString $ A.encode step)
      case resources.stepsFileHandle of
        Just handle -> liftIO $ hPutStrLn handle stepLine
        Nothing -> pure ()

    -- This wrapper supports the use of "printf" to format log messages.
    -- It prepends the current time to the message.
    -- It also reduces the need for OverloadedStrings.
    -- OverloadedStrings is not sufficient to get `printf` to work with the logger functions.
    logDebugStr _ str = do
      if logConfig.shouldLogDebugMessages
        then do
          currTime <- iso8601Show <$> liftIO getCurrentTime
          let msgToLog = currTime <> ": " <> str
          logDebugN (T.pack msgToLog)
        else pure ()

commaIfNotInit :: Step problem paving -> String
commaIfNotInit (InitStep _) = ""
commaIfNotInit _ = ","

bsToString :: ByteString -> String
bsToString bs =
  case TLE.decodeUtf8' bs of
    Left err -> "Failed to decode UTF8: " ++ (show err)
    Right stepT -> TL.unpack stepT
