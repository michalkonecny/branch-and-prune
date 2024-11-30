module BranchAndPrune.ForkUtils
  ( HasIsAborted (..),
    ThreadResources (..),
    initThreadResources,
    decideWhetherToFork,
    forkAndMerge,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, newTVar, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, toIO)

class HasIsAborted result where
  isAborted :: result -> Bool

data ThreadResources = ThreadResources
  { numberOfThreadsTV :: TVar Int,
    nextThreadNumber :: TVar Int
  }

initThreadResources :: IO ThreadResources
initThreadResources = do
  numberOfThreadsTV <- atomically $ newTVar 1
  nextThreadNumber <- atomically $ newTVar 1
  return $ ThreadResources {..}

getNextTwoThreadNumbers :: ThreadResources -> IO (Int, Int)
getNextTwoThreadNumbers (ThreadResources {..}) = atomically $ do
  n <- readTVar nextThreadNumber
  writeTVar nextThreadNumber (n + 2)
  pure (n, n+1)

decideWhetherToFork ::
  (MonadIO m) =>
  ThreadResources ->
  Int ->
  Maybe a ->
  m (Maybe a)
decideWhetherToFork (ThreadResources {..}) maxThreads maybeForkPossible =
  do
    liftIO $ atomically $ do
      nOfThreads <- readTVar numberOfThreadsTV
      case maybeForkPossible of
        Just forkInfo | nOfThreads < maxThreads -> do
          writeTVar numberOfThreadsTV (nOfThreads + 1)
          pure (Just forkInfo)
        _ -> pure Nothing

forkAndMerge ::
  (MonadUnliftIO m, HasIsAborted result, Semigroup result) =>
  ThreadResources ->
  (Int -> m result) ->
  (Int -> m result) ->
  m result
forkAndMerge tr@(ThreadResources {..}) compL compR =
  do
    (threaddNumber1, threaddNumber2) <- liftIO (getNextTwoThreadNumbers tr)
    compL_IO <- toIO (compL threaddNumber1)
    compR_IO <- toIO (compR threaddNumber2)
    liftIO $
      do
        -- create shared variables for results:
        resultL_Var <- newTVarIO Nothing
        resultR_Var <- newTVarIO Nothing

        -- create a shared variable for aborting the computation:
        abort_Var <- newTVarIO Nothing

        -- fork the computations:
        thread1 <- forkComp abort_Var resultL_Var compL_IO
        thread2 <- forkComp abort_Var resultR_Var compR_IO

        -- wait for either an abort or both threads to complete:
        result <- atomically $ do
          maybeAbortResult <- readTVar abort_Var
          case maybeAbortResult of
            Just result ->
              -- one of the threads aborted
              pure result -- pass on the aborted result
            _ -> do
              maybeResultL <- readTVar resultL_Var
              maybeResultR <- readTVar resultR_Var
              case (maybeResultL, maybeResultR) of
                (Just resultL, Just resultR) ->
                  -- both results available
                  pure $ resultL <> resultR -- merge the results
                _ ->
                  retry -- continue waiting
                  -- kill threads if aborted
        when (isAborted result) $ do
          killThread thread1
          killThread thread2

        pure result
  where
    forkComp abort_Var result_Var comp_IO =
      forkIO $
        do
          result <- comp_IO
          atomically $ do
            -- decrement thread counter:
            nOfThreads <- readTVar numberOfThreadsTV
            writeTVar numberOfThreadsTV (nOfThreads - 1)
            -- pass on the result:
            if isAborted result
              then abort_Var `writeTVar` Just result
              else result_Var `writeTVar` Just result
