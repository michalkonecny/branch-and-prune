module BranchAndPrune.Sets
  ( IsSet (..),
    SetFromBasic (..),
    CanSplitSet (..),
    CanPrune (..),
    Paving (..),
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
  )
where
import Text.Printf (printf)

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

data Paving set = Paving
  { inner :: set,
    undecided :: set,
    outer :: set
  }
  deriving (Eq, Show)

showPavingSummary :: (IsSet set) => Paving set -> String
showPavingSummary (Paving {..}) =
  printf "{inner: %s, undecided: %s, outer: %s}" (setShowStats inner) (setShowStats undecided) (setShowStats outer)
  -- printf "{inner: ??, undecided: %s, outer: %s}" (setShowStats undecided) (setShowStats outer)
  -- disabling printing of "inner" to avoid inconsistent slow-down when running with 1 thread

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

