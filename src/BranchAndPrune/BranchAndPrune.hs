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
import BranchAndPrune.Logging (BPLogConfig (..))
import BranchAndPrune.Sets
import BranchAndPrune.Steps
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Aeson qualified as A
import GHC.Conc (newTVarIO)
import LoggingFunctions (HasLoggingFunctions (..), LoggingFunctions (..))
import Text.Printf (printf)

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
    Show set,
    Show constraint,
    A.ToJSON basicSet,
    A.ToJSON set
  ) =>
  BPLogConfig ->
  Params m basicSet set priorityQueue constraint ->
  m (Result set priorityQueue)
branchAndPruneM logConfig (Params {..} :: Params m basicSet set priorityQueue constraint) =
  do
    -- initialise process resources
    numberOfThreadsTV <- liftIO $ newTVarIO 1
    logResources <- initLogging
    -- run the process
    result <- bpProcess logResources numberOfThreadsTV
    -- finalise process resources
    finaliseLogging logResources
    pure result
  where
    LoggingFunctions {..} = getLoggingFunctions logConfig
    bpProcess logResources numberOfThreadsTV = do
      logDebugStrR "B&P process starting"
      logStepR $ InitStep {scope, constraint = show constraint}

      let initQueue = singletonQueue (scope, constraint)
      result <- steps 0 emptyPaving initQueue

      logDebugStrR "B&P process finishing"
      logStepR $ DoneStep

      pure result
      where
        logStepR = logStep logResources
        logDebugStrR = logDebugStr logResources

        steps :: Int -> Paving set -> priorityQueue -> m (Result set priorityQueue)
        steps threadNumber pavingSoFar queue =
          -- first check if the paving allows us to abort (e.g. a model has been found)
          case shouldAbort pavingSoFar of
            Just abortDetail ->
              do
                -- abort
                logStepR $ AbortStep {detail = abortDetail}
                logDebugStrT $ "Aborting the B&P process: " ++ abortDetail
                pure $ Result {paving = pavingSoFar, queue, aborted = True}
            _ ->
              -- pick the next sub-problem from the queue (if any)
              case queuePickNext queue of
                Nothing ->
                  do
                    -- done - fully paved this thread's portion
                    logDebugStrT "The queue is empty, finishing this thread."
                    pure $ Result {paving = pavingSoFar, queue, aborted = False}
                Just ((b, c), queuePicked) ->
                  do
                    logDebugStrT $ printf "Picked set %s, constraint %s" (show b) (show c)
                    -- check if we should dig deeper into the next basic set or give up
                    if shouldGiveUpOnBasicSet b
                      then do
                        -- report giving up
                        logDebugStrT $ printf "Leaving undecided on: %s" (show b)
                        logStepR $ GiveUpOnSetStep {scope = b, constraint = show c}

                        -- register the basic set as undecided and continue
                        let undecidedWithB = pavingSoFar.undecided `setUnion` fromBasicSets [b]
                        let pavingWithB = pavingSoFar {undecided = undecidedWithB}
                        steps threadNumber pavingWithB queuePicked
                      else do
                        -- pruning the next basic set
                        logDebugStrT $ printf "Pruning on: %s" (show b)
                        (cPruned, prunePaving) <- pruneBasicSetM c b
                        let pavingWithPruningDecided = pavingSoFar `pavingAddDecided` prunePaving
                        logStepR $
                          PruneStep {scope = b, constraint = show c, prunedScope = prunePaving, prunedConstraint = show cPruned}
                        
                        -- check if the pruning fully decided the sub-problem
                        if setIsEmpty prunePaving.undecided
                          then do
                            -- done on b
                            logDebugStrT $ printf "Fully solved on %s: %s" (show b) (show prunePaving)

                            -- add b to the paving and continue
                            steps threadNumber pavingWithPruningDecided queuePicked
                          else do
                            -- not done on b, splitting the undecided subset of b
                            let pruneUndecidedSplit = splitSet prunePaving.undecided
                            logDebugStrT $ printf "Adding to queue: %s" (show pruneUndecidedSplit)
                            logStepR $ SplitStep {scope = b, pieces = pruneUndecidedSplit}

                            -- add the subset left-over from pruning to the queue
                            let queueWithPruningUndecided = queuePicked `queueAddMany` map (,cPruned) pruneUndecidedSplit
                            
                            -- is there capacity to split the job among two threads?
                            forkInfo <- decideWhetherToFork numberOfThreadsTV maxThreads (queueSplit queueWithPruningUndecided)
                            case forkInfo of
                              Just (queueL, queueR) ->
                                do
                                  -- yes, fork the queue between two new threads
                                  logDebugStrT "Forking queue into two queues to process independently"
                                  let compL = steps (threadNumber + 1) emptyPaving queueL
                                  let compR = steps (threadNumber + 2) emptyPaving queueR

                                  -- This is what the forkAndMerge does, but in parallel:
                                  --    resultL <- compL
                                  --    resultR <- compR
                                  --    let result = resultL <> resultR
                                  result <- forkAndMerge numberOfThreadsTV compL compR
                                  pure $ baseResultOnPrevPaving pavingWithPruningDecided result
                              _ ->
                                -- do not fork, continue with the current thread only
                                steps threadNumber pavingWithPruningDecided queueWithPruningUndecided
          where
            logDebugStrT (s :: String) = do
              logDebugStr logResources $ "Thread " ++ show threadNumber ++ ":" ++ s
