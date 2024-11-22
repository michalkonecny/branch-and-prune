{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module BranchAndPrune.LogUtils
  (
    logDebugStr
  )
where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger, logDebugN)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

-- The following wrapper supports the use of "printf" to format log messages.
-- It prepends the current time to the message.
-- It also reduced the need for OverloadedStrings.
-- OverloadedStrings is not sufficient to get `printf` to work with the logger functions.
logDebugStr :: (MonadIO m, MonadLogger m) => String -> m ()
logDebugStr str = do
  currTime <- iso8601Show <$> liftIO getCurrentTime
  let msgToLog = currTime <> ": " <> str
  logDebugN (T.pack msgToLog)

