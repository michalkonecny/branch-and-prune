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
    CanPruneM (..),
    CanSplitSet (..),
    Paving (..),
    pavingInner,
    pavingOuter,
    pavingUndecided,
    pavingInnerOuter,
    pavingInnerUndecided,
    pavingOuterUndecided,
    IsPriorityQueue (..),
    ParamsM (..),
    branchAndPruneM,
  )
where

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
  setUnion :: set -> set -> set

class SetFromBasic basicSet set where
  fromBasicSets :: [basicSet] -> set

class CanSplitSet basicSet set where
  splitSet :: set -> [basicSet] -- at least two so that B&P makes progress

class CanPruneM m basicSet constraint set where
  pruneBasicSetM :: constraint -> basicSet -> m (constraint, Paving set)

class IsPriorityQueue priorityQueue elem | priorityQueue -> elem where
  singletonQueue :: elem -> priorityQueue
  queueToList :: priorityQueue -> [elem]
  queuePickNext :: priorityQueue -> Maybe (elem, priorityQueue)
  queueAddMany :: priorityQueue -> [elem] -> priorityQueue

data Paving set = Paving
  { inner :: set,
    undecided :: set,
    outer :: set
  }
  deriving (Eq, Show)

emptyPaving :: (IsSet set) => Paving set
emptyPaving =
  Paving
    { inner = emptySet,
      undecided = emptySet,
      outer = emptySet
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

data ParamsM m basicSet set priorityQueue constraint = ParamsM
  { scope :: basicSet,
    constraint :: constraint,
    goalReached :: Paving set -> Bool,
    shouldGiveUpOnSet :: set -> Bool,
    dummyPriorityQueue :: priorityQueue,
    dummyMaction :: m ()
  }

branchAndPruneM ::
  ( IsSet set,
    SetFromBasic basicSet set,
    CanSplitSet basicSet set,
    CanPruneM m basicSet constraint set,
    IsPriorityQueue priorityQueue (basicSet, constraint),
    MonadLogger m,
    Show set,
    Show basicSet,
    Show constraint
  ) =>
  ParamsM m basicSet set priorityQueue constraint ->
  m (Paving set)
branchAndPruneM (ParamsM {..} :: ParamsM m basicSet set priorityQueue constraint) =
  let initQueue = singletonQueue (scope, constraint) :: priorityQueue
   in step emptyPaving initQueue
  where
    step :: Paving set -> priorityQueue -> m (Paving set)
    step pavingSoFar queue
      | goalReached pavingSoFar = do
          logDebugStr "Goal reached, finishing."
          let queueAsSet = fromBasicSets (map fst $ queueToList queue)
          pure $ pavingSoFar {undecided = pavingSoFar.undecided `setUnion` queueAsSet}
      | otherwise =
          case queuePickNext queue of
            Nothing -> do
              logDebugStr "The queue is empty, finishing."
              pure pavingSoFar
            Just ((b, c), queuePicked) -> do
              logDebugStr $ printf "Picked set %s, constraint %s" (show b) (show c)
              (cPruned, prunePaving) <- pruneBasicSetM c b
              logDebugStr $ printf "Pruning result: paving %s, constraint %s" (show prunePaving) (show cPruned)
              let pavingAfterPruning = pavingSoFar `pavingAddDecided` prunePaving
              let pavingWithPruningUndecided = pavingAfterPruning {undecided = pavingSoFar.undecided `setUnion` prunePaving.undecided}
              let pruneUndecidedSplit = splitSet prunePaving.undecided
              let queueWithPruningUndecided = queuePicked `queueAddMany` (map (,cPruned) pruneUndecidedSplit)
              if setIsEmpty prunePaving.undecided
                then do
                  logDebugStr $ printf "Fully solved on: %s" (show b)
                  step pavingAfterPruning queuePicked
                else
                  if shouldGiveUpOnSet prunePaving.undecided
                    then do
                      logDebugStr $ printf "Leaving undecided on: %s" (show prunePaving.undecided)
                      step pavingWithPruningUndecided queuePicked
                    else do
                      logDebugStr $ printf "Adding to queue: %s" (show pruneUndecidedSplit)
                      step pavingAfterPruning queueWithPruningUndecided