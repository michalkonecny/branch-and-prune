{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module BranchAndPrune.Logging (BPLogConfig (..), defaultBPLogConfig, getLoggingFunctions) where

import BranchAndPrune.BranchAndPrune (LoggingFunctions (..))
import BranchAndPrune.Steps (Step (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.Aeson qualified as A
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.Redis (runRedis)
import Database.Redis qualified as Redis
import System.IO (Handle, IOMode (WriteMode), hClose, hPutStrLn, openFile)

data BPLogConfig = BPLogConfig
  { shouldLogDebugMessages :: Bool,
    stepsFile :: Maybe FilePath,
    stepsRedisKey :: Maybe String
  }
  deriving (Eq)

defaultBPLogConfig :: BPLogConfig
defaultBPLogConfig =
  BPLogConfig
    { shouldLogDebugMessages = False,
      stepsFile = Nothing,
      stepsRedisKey = Nothing
    }

data BPLogResources = LogResources
  { stepsFileHandle :: Maybe Handle,
    redisDesination :: Maybe RedisDestination
  }

data RedisDestination = RedisDestination
  { connection :: Redis.Connection,
    streamKey :: BSS.ByteString
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
      redisDesination <- case logConfig.stepsRedisKey of
        Just streamKeyS -> liftIO $ do
          let streamKey = stringToBSS streamKeyS -- encode String to ByteString
          connection <- Redis.checkedConnect Redis.defaultConnectInfo
          liftIO $ runRedis connection $ do
            _ <- Redis.del [streamKey]
            pure ()
          pure $ Just $ RedisDestination {..}
        Nothing -> pure Nothing
      pure $ LogResources {..}

    finaliseLogging resources = do
      case resources.stepsFileHandle of
        Just handle -> liftIO $ do
          hPutStrLn handle "]"
          hClose handle
        _ -> pure ()
      case resources.redisDesination of
        Just (RedisDestination {..}) -> liftIO $ do
          Redis.disconnect connection
        _ -> pure ()

    logStep resources step = do
      let stepJSONBSS = BSL.toStrict $ A.encode step
      let stepLine = (commaIfNotInit step) <> (bssToString stepJSONBSS)
      case resources.stepsFileHandle of
        Just handle -> liftIO $ hPutStrLn handle stepLine
        Nothing -> pure ()
      case resources.redisDesination of
        Just (RedisDestination {..}) -> liftIO $ runRedis connection $ do
          _ <- redisAddStep streamKey stepJSONBSS
          pure ()
        _ -> pure ()

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

redisAddStep :: _ => BSS.ByteString -> BSS.ByteString -> _
redisAddStep streamKey stepJSONBSS =
  Redis.xadd streamKey (stringToBSS "*") [(stringToBSS "step", stepJSONBSS)]

bssToString :: BSS.ByteString -> String
bssToString bs =
  case TE.decodeUtf8' bs of
    Left err -> "Failed to decode UTF8: " ++ (show err)
    Right stepT -> T.unpack stepT

stringToBSS :: String -> BSS.ByteString
stringToBSS = TE.encodeUtf8 . T.pack
