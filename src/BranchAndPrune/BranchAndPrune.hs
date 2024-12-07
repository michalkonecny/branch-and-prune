{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BranchAndPrune.BranchAndPrune
  ( module BranchAndPrune.Sets,
    module BranchAndPrune.Paving,
    IsPriorityQueue (..),
    Params (..),
    Result (..),
    branchAndPruneM,
  )
where

import BranchAndPrune.ForkUtils (HasIsAborted (..), decideWhetherToFork, forkAndMerge, initThreadResources)
import BranchAndPrune.Logging (BPLogConfig (..))
import BranchAndPrune.Paving
import BranchAndPrune.Sets
import BranchAndPrune.Steps
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Aeson qualified as A
import Data.Maybe (isJust)
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
  { problem :: Problem constraint basicSet,
    shouldAbort :: Paving constraint basicSet set -> Maybe String,
    shouldGiveUpSolvingProblem :: Problem constraint basicSet -> Bool,
    maxThreads :: Int,
    dummyPriorityQueue :: priorityQueue
  }

data Result constraint basicSet set = Result
  { paving :: Paving constraint basicSet set,
    aborted :: Maybe String
  }

instance HasIsAborted (Result constraint basicSet set) where
  isAborted result = isJust result.aborted

instance
  (IsSet set) =>
  Semigroup (Result constraint basicSet set)
  where
  (<>) resL resR =
    Result
      { paving = resL.paving `pavingMerge` resR.paving,
        aborted = case (resL.aborted, resR.aborted) of
          (Just detailL, Just detailR) -> Just (detailL <> "; " <> detailR)
          (Just detailL, _) -> Just detailL
          (_, Just detailR) -> Just detailR
          _ -> Nothing
      }

baseResultOnPrevPaving ::
  (IsSet set) =>
  Paving constraint basicSet set ->
  Result constraint basicSet set ->
  Result constraint basicSet set
baseResultOnPrevPaving prevPaving res =
  res {paving = prevPaving `pavingMerge` res.paving}

branchAndPruneM ::
  ( IsSet set,
    CanSplitProblem constraint basicSet,
    CanPrune m constraint basicSet set,
    IsPriorityQueue priorityQueue (Problem constraint basicSet),
    MonadLogger m,
    MonadUnliftIO m,
    Show constraint,
    Show basicSet,
    Show set,
    A.ToJSON constraint,
    A.ToJSON basicSet,
    A.ToJSON set
  ) =>
  BPLogConfig ->
  Params m basicSet set priorityQueue constraint ->
  m (Result constraint basicSet set)
branchAndPruneM logConfig (Params {problem = initialProblem, ..} :: Params m basicSet set priorityQueue constraint) =
  do
    -- initialise process resources
    threadResources <- liftIO $ initThreadResources
    logResources <- initLogging

    -- run the process
    result <- bpProcess logResources threadResources

    -- finalise process resources
    finaliseLogging logResources
    pure result
  where
    LoggingFunctions {..} = getLoggingFunctions logConfig
    bpProcess logResources threadResources = do
      logDebugStrR "B&P process starting"
      logStepR $ InitStep {problem = initialProblem}

      -- start the recursive and parallel paving process with initial problem in the queue
      let initQueue = singletonQueue initialProblem
      result <- steps 0 (emptyPaving initialProblem.scope) initQueue

      logDebugStrR "B&P process finishing"
      logStepR $ DoneStep

      pure result
      where
        logStepR = logStep logResources
        logDebugStrR = logDebugStr logResources

        steps :: Int -> Paving constraint basicSet set -> priorityQueue -> m (Result constraint basicSet set)
        steps threadNumber pavingSoFar queue =
          -- first check if the paving allows us to abort (e.g. a model has been found)
          case shouldAbort pavingSoFar of
            Just abortDetail ->
              do
                -- abort
                logStepR $ AbortStep {detail = abortDetail}
                logDebugStrT $ "Aborting the B&P process: " ++ abortDetail
                pure $ Result {paving = pavingSoFar, aborted = Just abortDetail}
            _ ->
              -- pick the next sub-problem from the queue (if any)
              case queuePickNext queue of
                Nothing ->
                  do
                    -- done - fully paved this thread's portion
                    logDebugStrT "The queue is empty, finishing this thread."
                    pure $ Result {paving = pavingSoFar, aborted = Nothing}
                Just (problem, queuePicked) ->
                  do
                    logDebugStrT $ printf "Picked problem: %s" (show problem)
                    -- check if we should dig deeper into the next basic set or give up
                    if shouldGiveUpSolvingProblem problem
                      then do
                        -- report giving up
                        logDebugStrT $ printf "Leaving problem undecided on: %s" (show problem.scope)
                        logStepR $ GiveUpOnProblemStep {problem}

                        -- register problem as undecided and continue
                        let undecidedWithP = problem : pavingSoFar.undecided
                        let pavingWithP = pavingSoFar {undecided = undecidedWithP}
                        steps threadNumber pavingWithP queuePicked
                      else do
                        -- pruning the next basic set
                        logDebugStrT $ printf "Pruning on: %s" (show problem)
                        prunePaving <- pruneProblemM problem
                        logStepR $ PruneStep {problem, prunePaving}

                        -- register what the pruning decided
                        let pavingWithPruningDecided = pavingSoFar `pavingAddDecided` prunePaving

                        -- check if the pruning fully decided the sub-problem
                        if null prunePaving.undecided
                          then do
                            -- fully solved problem
                            logDebugStrT $ printf "Fully solved %s: %s" (show problem) (show prunePaving)

                            -- continue
                            steps threadNumber pavingWithPruningDecided queuePicked
                          else do
                            -- problem not fully solved by pruning, splitting the undecided sub-problems
                            let pruneUndecidedSplit = case prunePaving.undecided of
                                  [p] -> splitProblem p
                                  problems -> problems
                            logDebugStrT $ printf "Adding to queue: %s" (show pruneUndecidedSplit)
                            logStepR $ SplitStep {problem, pieces = pruneUndecidedSplit}

                            -- add the subset left-over from pruning to the queue
                            let queueWithPruningUndecided = queuePicked `queueAddMany` pruneUndecidedSplit

                            -- is there capacity to split the job among two threads?
                            forkInfo <- decideWhetherToFork threadResources maxThreads (queueSplit queueWithPruningUndecided)
                            case forkInfo of
                              Just (queueL, queueR) ->
                                do
                                  -- yes, fork the queue between two new threads
                                  logDebugStrT "Forking queue into two queues to process independently"
                                  let compL = \threadNumberL -> steps threadNumberL (emptyPaving pavingSoFar.scope) queueL
                                  let compR = \threadNumberR -> steps threadNumberR (emptyPaving pavingSoFar.scope) queueR

                                  -- This is what the forkAndMerge does, but in parallel:
                                  --    resultL <- compL
                                  --    resultR <- compR
                                  --    let result = resultL <> resultR
                                  result <- forkAndMerge threadResources compL compR
                                  pure $ baseResultOnPrevPaving pavingWithPruningDecided result
                              _ ->
                                -- do not fork, continue with the current thread only
                                steps threadNumber pavingWithPruningDecided queueWithPruningUndecided
          where
            logDebugStrT (s :: String) = do
              logDebugStr logResources $ "Thread " ++ show threadNumber ++ ":" ++ s
