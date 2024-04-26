{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BranchAndPrune
  ( IsSet (..),
    SetFromBasic (..),
    CanPrune (..),
    CanSplitSet (..),
    Paving (..),
    pavingInner,
    pavingOuter,
    pavingUndecided,
    pavingInnerOuter,
    pavingInnerUndecided,
    pavingOuterUndecided,
    IsPriorityQueue (..),
    Params (..),
    Result (..),
    branchAndPruneM,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Control.Monad.Logger (MonadLogger, logDebugN)
import qualified Data.Text as T
import Text.Printf (printf)

-- The following wrapper supports the use of "printf" to format log messages.
-- It also reduced the need for OverloadedStrings.
-- OverloadedStrings is not sufficient to get `printf` to work with the logger functions.
logDebugStr :: (MonadLogger m) => String -> m ()
logDebugStr = logDebugN . T.pack

class IsSet set where
  emptySet :: set
  setIsEmpty :: set -> Bool
  setShowStats :: set -> String
  setUnion :: set -> set -> set

class SetFromBasic basicSet set where
  fromBasicSets :: [basicSet] -> set

class CanSplitSet basicSet set where
  splitSet :: set -> [basicSet] -- at least two so that B&P makes progress

class CanPrune m basicSet constraint set where
  pruneBasicSetM :: constraint -> basicSet -> m (constraint, Paving set)

class IsPriorityQueue priorityQueue elem | priorityQueue -> elem where
  singletonQueue :: elem -> priorityQueue
  queueToList :: priorityQueue -> [elem]
  queuePickNext :: priorityQueue -> Maybe (elem, priorityQueue)
  queueAddMany :: priorityQueue -> [elem] -> priorityQueue
  queueSplit :: priorityQueue -> Maybe (priorityQueue, priorityQueue)
  queueMerge :: priorityQueue -> priorityQueue -> priorityQueue

data Paving set = Paving
  { inner :: set,
    undecided :: set,
    outer :: set
  }
  deriving (Eq, Show)

showPavingSummary :: (IsSet set) => Paving set -> String
showPavingSummary (Paving {..}) =
  printf "{inner: %s, undecided: %s, outer: %s}" (setShowStats inner) (setShowStats undecided) (setShowStats outer)

emptyPaving :: (IsSet set) => Paving set
emptyPaving =
  Paving
    { inner = emptySet,
      undecided = emptySet,
      outer = emptySet
    }

pavingMerge :: (IsSet set) => Paving set -> Paving set -> Paving set
pavingMerge paving1 paving2 =
  paving1
    { inner = paving1.inner `setUnion` paving2.inner,
      undecided = paving1.undecided `setUnion` paving2.undecided,
      outer = paving1.outer `setUnion` paving2.outer
    }

pavingAddDecided :: (IsSet set) => Paving set -> Paving set -> Paving set
pavingAddDecided paving1 paving2 =
  paving1
    { inner = paving1.inner `setUnion` paving2.inner,
      outer = paving1.outer `setUnion` paving2.outer
    }

pavingInner :: (IsSet set) => set -> Paving set
pavingInner inner = Paving {inner, undecided = emptySet, outer = emptySet}

pavingOuter :: (IsSet set) => set -> Paving set
pavingOuter outer = Paving {inner = emptySet, undecided = emptySet, outer}

pavingUndecided :: (IsSet set) => set -> Paving set
pavingUndecided undecided = Paving {undecided, inner = emptySet, outer = emptySet}

pavingInnerOuter :: (IsSet set) => set -> set -> Paving set
pavingInnerOuter inner outer = Paving {inner, undecided = emptySet, outer}

pavingInnerUndecided :: (IsSet set) => set -> set -> Paving set
pavingInnerUndecided inner undecided = Paving {inner, undecided, outer = emptySet}

pavingOuterUndecided :: (IsSet set) => set -> set -> Paving set
pavingOuterUndecided outer undecided = Paving {inner = emptySet, undecided, outer}

data Params m basicSet set priorityQueue constraint = ParamsM
  { scope :: basicSet,
    constraint :: constraint,
    shouldAbort :: Paving set -> Bool,
    shouldGiveUpOnBasicSet :: basicSet -> Bool,
    maxForkDepth :: Int,
    dummyPriorityQueue :: priorityQueue,
    dummyMaction :: m ()
  }

data Result set priorityQueue = Result
  { paving :: Paving set,
    queue :: priorityQueue
  }

mergeResults ::
  (IsSet set, IsPriorityQueue priorityQueue elem0) =>
  Result set priorityQueue ->
  Result set priorityQueue ->
  Result set priorityQueue
mergeResults (Result pavingL queueL) (Result pavingR queueR) =
  Result (pavingL `pavingMerge` pavingR) (queueL `queueMerge` queueR)

baseResultOnPrevPaving ::
  (IsSet set) =>
  Paving set ->
  Result set priorityQueue ->
  Result set priorityQueue
baseResultOnPrevPaving prevPaving (Result paving queue) =
  Result (prevPaving `pavingMerge` paving) queue

branchAndPruneM ::
  ( IsSet set,
    SetFromBasic basicSet set,
    CanSplitSet basicSet set,
    CanPrune m basicSet constraint set,
    IsPriorityQueue priorityQueue (basicSet, constraint),
    MonadLogger m,
    MonadUnliftIO m,
    Show basicSet,
    Show constraint
  ) =>
  Params m basicSet set priorityQueue constraint ->
  m (Result set priorityQueue)
branchAndPruneM (ParamsM {..} :: Params m basicSet set priorityQueue constraint) =
  do
    logDebugStr "B&P process starting"
    result@(Result {paving, queue}) <- bpProcess
    logDebugStr "B&P process finishing"
    -- print result summary:
    case queuePickNext queue of
      Nothing ->
        do
          -- queue empty, process finished
          logDebugStr "Successfully paved the whole set."
          logDebugStr $ printf "Paving summary: %s" (showPavingSummary paving)
      Just ((b, _), _) ->
        -- queue not empty, process unfinished
        logDebugStr $ printf "Aborted around: %s" (show b)
    pure result
  where
    bpProcess = step 0 emptyPaving initQueue
      where
        initQueue = singletonQueue (scope, constraint)
    step :: Int -> Paving set -> priorityQueue -> m (Result set priorityQueue)
    step forkDepth pavingSoFar queue
      | shouldAbort pavingSoFar =
          do
            logDebugStr "Stopping the B&P process."
            pure $ Result {paving = pavingSoFar, queue}
      | otherwise =
          case queuePickNext queue of
            Nothing ->
              do
                logDebugStr "The queue is empty, finishing."
                pure $ Result {paving = pavingSoFar, queue}
            Just ((b, c), queuePicked) ->
              do
                logDebugStr $ printf "Picked set %s, constraint %s" (show b) (show c)
                if shouldGiveUpOnBasicSet b
                  then do
                    logDebugStr $ printf "Leaving undecided on: %s" (show b)
                    let undecidedWithB = pavingSoFar.undecided `setUnion` (fromBasicSets [b])
                    let pavingWithB = pavingSoFar {undecided = undecidedWithB}
                    step forkDepth pavingWithB queuePicked
                  else do
                    -- not giving up on b
                    logDebugStr $ printf "Pruning on: %s" (show b)
                    (cPruned, prunePaving) <- pruneBasicSetM c b
                    --
                    let pavingWithPruningDecided = pavingSoFar `pavingAddDecided` prunePaving
                    if setIsEmpty prunePaving.undecided
                      then do
                        -- done on b
                        logDebugStr $ printf "Fully solved on: %s" (show b)
                        step forkDepth pavingWithPruningDecided queuePicked
                      else do
                        -- not done on b
                        let pruneUndecidedSplit = splitSet prunePaving.undecided
                        logDebugStr $ printf "Adding to queue: %s" (show pruneUndecidedSplit)
                        let queueWithPruningUndecided = queuePicked `queueAddMany` (map (,cPruned) pruneUndecidedSplit)
                        case queueSplit queueWithPruningUndecided of
                          Just (queueL, queueR) | forkDepth < maxForkDepth ->
                            do
                              logDebugStr "Forking queue into two queues to process independently"
                              let compL = step (forkDepth + 1) emptyPaving queueL
                              let compR = step (forkDepth + 1) emptyPaving queueR
                              result <- forkAndMerge compL compR
                              pure $ baseResultOnPrevPaving pavingWithPruningDecided result
                          _ ->
                            do
                              step forkDepth pavingWithPruningDecided queueWithPruningUndecided

    forkAndMerge :: m (Result set priorityQueue) -> m (Result set priorityQueue) -> m (Result set priorityQueue)
    forkAndMerge compL compR =
      do
        -- TODO: parallise
        resultL <- compL
        resultR <- compR
        pure $ mergeResults resultL resultR
