module BranchAndPrune.Paving
  ( Problem (..),
    Paving (..),
    CanPrune (..),
    showPavingSummary,
    emptyPaving,
    pavingMerge,
    pavingAddDecided,
    pavingInner,
    pavingOuter,
    pavingUndecided,
    pavingInnerOuter,
    pavingInnerUndecided,
    pavingOuterUndecided,
    pavingHasInfo,
  )
where

import BranchAndPrune.Sets
import GHC.Generics
import Text.Printf (printf)

data Paving constraint basicSet set = Paving
  { scope :: basicSet,
    inner :: set,
    undecided :: [Problem constraint basicSet],
    outer :: set
  }
  deriving (Show, Generic)

class CanPrune m constraint basicSet set where
  pruneProblemM :: Problem constraint basicSet -> m (Paving constraint basicSet set)

showPavingSummary ::
  ( ShowStats (Subset set basicSet),
    BasicSetsToSet basicSet set
  ) =>
  Paving constraint basicSet set ->
  String
showPavingSummary (Paving {..}) =
  printf "{inner: %s, undecided: %s, outer: %s}" (showSt inner) (showSt undecidedSet) (showSt outer)
  where
    showSt set = showStats (Subset set scope)
    undecidedSet = basicSetsToSet $ map (\p -> p.scope) undecided

-- printf "{inner: ??, undecided: %s, outer: %s}" (setShowStats undecided) (setShowStats outer)
-- disabling printing of "inner" to avoid inconsistent slow-down when running with 1 thread

emptyPaving :: (IsSet set) => basicSet -> Paving constraint basicSet set
emptyPaving scope =
  Paving
    { scope,
      inner = emptySet,
      undecided = [],
      outer = emptySet
    }

pavingHasInfo :: (IsSet set) => Paving constraint basicSet set -> Bool
pavingHasInfo paving =
  not (setIsEmpty paving.inner)
    || not (setIsEmpty paving.outer)
    || length paving.undecided > 1

pavingMerge ::
  (IsSet set) =>
  Paving constraint basicSet set ->
  Paving constraint basicSet set ->
  Paving constraint basicSet set
pavingMerge paving1 paving2 =
  paving1
    { inner = paving1.inner `setUnion` paving2.inner,
      undecided = paving1.undecided ++ paving2.undecided,
      outer = paving1.outer `setUnion` paving2.outer
    }

pavingAddDecided ::
  (IsSet set) =>
  Paving constraint basicSet set ->
  Paving constraint basicSet set ->
  Paving constraint basicSet set
pavingAddDecided paving1 paving2 =
  paving1
    { inner = paving1.inner `setUnion` paving2.inner,
      outer = paving1.outer `setUnion` paving2.outer
    }

pavingInner :: (IsSet set) => basicSet -> set -> Paving constraint basicSet set
pavingInner scope inner = Paving {scope, inner, undecided = [], outer = emptySet}

pavingOuter :: (IsSet set) => basicSet -> set -> Paving constraint basicSet set
pavingOuter scope outer = Paving {scope, inner = emptySet, undecided = [], outer}

pavingUndecided :: (IsSet set) => basicSet -> [Problem constraint basicSet] -> Paving constraint basicSet set
pavingUndecided scope undecided = Paving {scope, undecided, inner = emptySet, outer = emptySet}

pavingInnerOuter :: basicSet -> set -> set -> Paving constraint basicSet set
pavingInnerOuter scope inner outer = Paving {scope, inner, undecided = [], outer}

pavingInnerUndecided :: (IsSet set) => basicSet -> set -> [Problem constraint basicSet] -> Paving constraint basicSet set
pavingInnerUndecided scope inner undecided = Paving {scope, inner, undecided, outer = emptySet}

pavingOuterUndecided :: (IsSet set) => basicSet -> set -> [Problem constraint basicSet] -> Paving constraint basicSet set
pavingOuterUndecided scope outer undecided = Paving {scope, inner = emptySet, undecided, outer}
