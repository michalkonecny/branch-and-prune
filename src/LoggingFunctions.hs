module LoggingFunctions
  ( 
    LoggingFunctions(..),
    HasLoggingFunctions(..)
  )
where

data LoggingFunctions resources step m = LoggingFunctions
  { initLogging :: m resources,
    logDebugStr :: resources -> String -> m (),
    logStep :: resources -> step -> m (),
    finaliseLogging :: resources -> m ()
  }

class (Monad m) => HasLoggingFunctions step m where
  type LoggingResources step
  type LoggingConfig step
  getLoggingFunctions :: (LoggingConfig step) -> LoggingFunctions (LoggingResources step) step m

