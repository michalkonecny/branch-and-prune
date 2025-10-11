module BranchAndPrune.ForkUtils
  ( HasIsAborted (..),
    MonadUnliftIOWithState (..),
    ThreadResources (..),
    initThreadResources,
    decideWhetherToFork,
    forkAndMerge,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Conc (ThreadId)

class HasIsAborted result where
  isAborted :: result -> Bool

data ThreadResources = ThreadResources
  { numberOfThreadsTV :: TVar Int,
    nextThreadNumber :: TVar Int
  }

initThreadResources :: IO ThreadResources
initThreadResources = do
  numberOfThreadsTV <- newTVarIO 1
  nextThreadNumber <- newTVarIO 1
  return $ ThreadResources {..}

getNextTwoThreadNumbers :: ThreadResources -> IO (Int, Int)
getNextTwoThreadNumbers (ThreadResources {..}) = atomically $ do
  n <- readTVar nextThreadNumber
  writeTVar nextThreadNumber (n + 2)
  pure (n, n + 1)

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

-- |
--  A monad that supports unlifting to IO while carrying some state that can be
--  passed to and from the IO computations.
class MonadUnliftIOWithState m where
  type MonadUnliftIOState m
  toIOWithState :: m b -> m (IO (b, MonadUnliftIOState m))
  absorbState :: MonadUnliftIOState m -> m ()

-- An instance for StateT monads could look like this:

-- instance (MonadUnliftIO m, Semigroup s) => MonadUnliftIOWithState (StateT s m) where
--   type MonadUnliftIOState (StateT s m) = s
--   toIOWithState mb = StateT $ \s -> do
--     withRunInIO $ \runInIO -> do
--       let ioWithS = runInIO (mb `runStateT` s)
--       pure (ioWithS, s)
--   absorbState s = StateT $ \s' -> pure ((), s <> s')

forkAndMerge ::
  (MonadUnliftIOWithState m, MonadIO m, HasIsAborted result, Semigroup result) =>
  ThreadResources ->
  (Int -> m result) ->
  (Int -> m result) ->
  m result
forkAndMerge tr@(ThreadResources {numberOfThreadsTV}) (compL :: Int -> m result) compR =
  do
    (threadNumber1, threadNumber2) <- liftIO (getNextTwoThreadNumbers tr)
    compL_IO <- toIOWithState (compL threadNumber1)
    compR_IO <- toIOWithState (compR threadNumber2)
    (result, states) <- liftIO $
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
        resultAndState@(result, _) <- atomically $ do
          maybeAbortResult <- readTVar abort_Var
          case maybeAbortResult of
            Just (result, s) ->
              -- one of the threads aborted
              pure (result, [s]) -- pass on the aborted result
            _ -> do
              maybeResultL <- readTVar resultL_Var
              maybeResultR <- readTVar resultR_Var
              case (maybeResultL, maybeResultR) of
                (Just (resultL, sL), Just (resultR, sR)) ->
                  -- both results available
                  pure (resultL <> resultR, [sL, sR]) -- merge the results
                _ ->
                  retry -- continue waiting
                  -- kill threads if aborted
        when (isAborted result) $ do
          killThread thread1
          killThread thread2

        pure resultAndState
    mapM_ absorbState states
    pure result
  where
    forkComp ::
      TVar (Maybe (result, MonadUnliftIOState m)) ->
      TVar (Maybe (result, MonadUnliftIOState m)) ->
      IO (result, MonadUnliftIOState m) ->
      IO ThreadId
    forkComp abort_Var result_Var comp_IO =
      forkIO $
        do
          resultAndState@(result, _) <- comp_IO
          atomically $ do
            -- decrement thread counter:
            nOfThreads <- readTVar numberOfThreadsTV
            writeTVar numberOfThreadsTV (nOfThreads - 1)
            -- pass on the result:
            if isAborted result
              then abort_Var `writeTVar` Just resultAndState
              else result_Var `writeTVar` Just resultAndState
