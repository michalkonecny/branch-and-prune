{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BranchAndPrune.BranchAndPrune
  ( module BranchAndPrune.Sets,
    IsPriorityQueue (..),
    Params (..),
    Result (..),
    branchAndPruneM,
  )
where

import BranchAndPrune.ForkUtils (HasIsAborted (..), decideWhetherToFork, forkAndMerge)
import BranchAndPrune.Sets
import BranchAndPrune.Steps
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson qualified as A
import GHC.Conc (newTVarIO)
import Text.Printf (printf)
import BranchAndPrune.Logging (LogConfig(..), LogFunctions (..), getLogFunctions)
import Control.Monad.Logger (MonadLogger)

class IsPriorityQueue priorityQueue elem | priorityQueue -> elem where
  singletonQueue :: elem -> priorityQueue
  queueToList :: priorityQueue -> [elem]
  queuePickNext :: priorityQueue -> Maybe (elem, priorityQueue)
  queueAddMany :: priorityQueue -> [elem] -> priorityQueue
  queueSplit :: priorityQueue -> Maybe (priorityQueue, priorityQueue)
  queueMerge :: priorityQueue -> priorityQueue -> priorityQueue

data Params m basicSet set priorityQueue constraint = Params
  { scope :: basicSet,
    constraint :: constraint,
    shouldAbort :: Paving set -> Maybe String,
    shouldGiveUpOnBasicSet :: basicSet -> Bool,
    maxThreads :: Integer,
    dummyPriorityQueue :: priorityQueue
  }

data Result set priorityQueue = Result
  { paving :: Paving set,
    queue :: priorityQueue,
    aborted :: Bool
  }

instance HasIsAborted (Result set priorityQueue) where
  isAborted result = result.aborted

instance
  (IsSet set, IsPriorityQueue priorityQueue elem) =>
  Semigroup (Result set priorityQueue)
  where
  (<>) resL resR =
    Result
      { paving = resL.paving `pavingMerge` resR.paving,
        queue = resL.queue `queueMerge` resR.queue,
        aborted = resL.aborted || resR.aborted
      }

baseResultOnPrevPaving ::
  (IsSet set) =>
  Paving set ->
  Result set priorityQueue ->
  Result set priorityQueue
baseResultOnPrevPaving prevPaving res =
  res {paving = prevPaving `pavingMerge` res.paving}

branchAndPruneM ::
  ( IsSet set,
    SetFromBasic basicSet set,
    CanSplitSet basicSet set,
    CanPrune m basicSet constraint set,
    IsPriorityQueue priorityQueue (basicSet, constraint),
    MonadLogger m,
    MonadUnliftIO m,
    Show basicSet,
    Show constraint,
    A.ToJSON basicSet,
    A.ToJSON set
  ) =>
  LogConfig ->
  Params m basicSet set priorityQueue constraint ->
  m (Result set priorityQueue)
branchAndPruneM logConfig (Params {..} :: Params m basicSet set priorityQueue constraint) =
  do
    numberOfThreadsTV <- liftIO $ newTVarIO 1
    logResources <- initLogging
    let logDebugStrR = logDebugStr logResources
    let reportStepR = reportStep logResources

    reportStepR $ InitStep {scope, constraint = show constraint}
    logDebugStrR "B&P process starting"
    result@(Result {queue}) <- bpProcess logResources numberOfThreadsTV
    logDebugStrR "B&P process finishing"

    -- print result summary:
    case queuePickNext queue of
      Nothing ->
        -- queue empty, process finished
        logDebugStrR "Successfully paved the whole set."
      Just ((b, _), _) ->
        -- queue not empty, process unfinished
        logDebugStrR $ printf "Aborted around: %s" (show b)

    reportStepR $ DoneStep
    finaliseLogging logResources
    pure result
  where
    LogFunctions {..} = getLogFunctions logConfig
    bpProcess logResources numberOfThreadsTV = do
      step 0 emptyPaving initQueue
      where
        reportStepR = reportStep logResources
        initQueue = singletonQueue (scope, constraint)
        step :: Int -> Paving set -> priorityQueue -> m (Result set priorityQueue)
        step threadNumber pavingSoFar queue =
          case shouldAbort pavingSoFar of
            Just abortDetail ->
              do
                -- abort
                reportStepR $ AbortStep {detail = abortDetail}
                logDebugThread "Stopping the B&P process."
                pure $ Result {paving = pavingSoFar, queue, aborted = True}
            _ ->
              case queuePickNext queue of
                Nothing ->
                  do
                    -- done - fully paved
                    logDebugThread "The queue is empty, finishing this thread."
                    pure $ Result {paving = pavingSoFar, queue, aborted = False}
                Just ((b, c), queuePicked) ->
                  do
                    logDebugThread $ printf "Picked set %s, constraint %s" (show b) (show c)
                    if shouldGiveUpOnBasicSet b
                      then do
                        -- give up
                        reportStepR $ GiveUpOnSetStep {scope = b, constraint = show c}
                        logDebugThread $ printf "Leaving undecided on: %s" (show b)
                        let undecidedWithB = pavingSoFar.undecided `setUnion` fromBasicSets [b]
                        let pavingWithB = pavingSoFar {undecided = undecidedWithB}
                        step threadNumber pavingWithB queuePicked
                      else do
                        -- pruning b
                        logDebugThread $ printf "Pruning on: %s" (show b)
                        (cPruned, prunePaving) <- pruneBasicSetM c b
                        reportStepR $
                          PruneStep {scope = b, constraint = show c, prunedScope = prunePaving, prunedConstraint = show cPruned}
                        --
                        let pavingWithPruningDecided = pavingSoFar `pavingAddDecided` prunePaving
                        if setIsEmpty prunePaving.undecided
                          then do
                            -- done on b
                            logDebugThread $ printf "Fully solved on: %s" (show b)
                            step threadNumber pavingWithPruningDecided queuePicked
                          else do
                            -- not done on b, splitting the undecided subset of b
                            let pruneUndecidedSplit = splitSet prunePaving.undecided
                            reportStepR $ SplitStep {scope = b, pieces = pruneUndecidedSplit}
                            logDebugThread $ printf "Adding to queue: %s" (show pruneUndecidedSplit)
                            let queueWithPruningUndecided = queuePicked `queueAddMany` map (,cPruned) pruneUndecidedSplit
                            forkInfo <- decideWhetherToFork numberOfThreadsTV maxThreads (queueSplit queueWithPruningUndecided)
                            case forkInfo of
                              Just (queueL, queueR) ->
                                do
                                  logDebugThread "Forking queue into two queues to process independently"
                                  let compL = step (threadNumber + 1) emptyPaving queueL
                                  let compR = step (threadNumber + 2) emptyPaving queueR

                                  -- This is what the forkAndMerge does, but in parallel:
                                  --    resultL <- compL
                                  --    resultR <- compR
                                  --    let result = resultL <> resultR
                                  result <- forkAndMerge numberOfThreadsTV compL compR
                                  pure $ baseResultOnPrevPaving pavingWithPruningDecided result
                              _ ->
                                -- do not fork, continue the current thread for the whole queue
                                do
                                  step threadNumber pavingWithPruningDecided queueWithPruningUndecided
          where
            logDebugThread (s :: String) = do
              logDebugStr logResources $ "Thread " ++ show threadNumber ++ ":" ++ s
