{-# LANGUAGE DisambiguateRecordFields #-}
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
    pavingInnerUndecided,
    pavingInner,
    pavingOuter,
    pavingOuterUndecided,
    IsPriorityQueue (..),
    Params (..),
    branchAndPrune,
  )
where

class IsSet set where
  emptySet :: set
  setIsEmpty :: set -> Bool
  setUnion :: set -> set -> set

class SetFromBasic basicSet set where
  fromBasicSets :: [basicSet] -> set

class CanSplitSet basicSet set where
  splitSet :: set -> [basicSet]

class CanPrune basicSet constraint set where
  pruneBasicSet :: constraint -> basicSet -> (constraint, Paving set)

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

pavingInnerUndecided :: (IsSet set) => set -> set -> Paving set
pavingInnerUndecided inner undecided = Paving {inner, undecided, outer = emptySet}

pavingOuterUndecided :: (IsSet set) => set -> set -> Paving set
pavingOuterUndecided outer undecided = Paving {inner = emptySet, undecided, outer}

data Params basicSet set priorityQueue constraint = Params
  { scope :: basicSet,
    constraint :: constraint,
    goalReached :: Paving set -> Bool,
    shouldGiveUpOnSet :: set -> Bool,
    dummyPriorityQueue :: priorityQueue
  }

branchAndPrune ::
  ( IsSet set,
    SetFromBasic basicSet set,
    CanSplitSet basicSet set,
    CanPrune basicSet constraint set,
    IsPriorityQueue priorityQueue (basicSet, constraint)
  ) =>
  Params basicSet set priorityQueue constraint ->
  Paving set
branchAndPrune (Params {..} :: Params basicSet set priorityQueue constraint) =
  let initQueue = singletonQueue (scope, constraint)
      _d = [initQueue, dummyPriorityQueue] -- help type inference
   in step emptyPaving initQueue
  where
    step :: Paving set -> priorityQueue -> Paving set
    step pavingSoFar queue =
      case queuePickNext queue of
        Just ((b, c), queuePicked)
          | not (goalReached pavingSoFar) ->
              let (cPruned, prunePaving) = pruneBasicSet c b
                  pavingAfterPruning = pavingSoFar `pavingAddDecided` prunePaving
                  pavingWithPruningUndecided = pavingAfterPruning {undecided = pavingSoFar.undecided `setUnion` prunePaving.undecided}
                  queueWithPruningUndecided = queuePicked `queueAddMany` (map (,cPruned) (splitSet prunePaving.undecided))
               in if setIsEmpty prunePaving.undecided
                    then step pavingAfterPruning queuePicked
                    else
                      if shouldGiveUpOnSet prunePaving.undecided
                        then step pavingWithPruningUndecided queuePicked
                        else step pavingAfterPruning queueWithPruningUndecided
        _ ->
          pavingSoFar {undecided = pavingSoFar.undecided `setUnion` queueAsSet}
          where
            queueAsSet = fromBasicSets (map fst $ queueToList queue)
