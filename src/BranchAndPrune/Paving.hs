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
  )
where

import BranchAndPrune.Sets
import Data.Aeson qualified as A
import GHC.Generics
import Text.Printf (printf)

data Paving constraint basicSet set = Paving
  { inner :: set,
    undecided :: [Problem constraint basicSet],
    outer :: set
  }
  deriving (Show, Generic)

instance (A.ToJSON set, A.ToJSON basicSet, A.ToJSON constraint) => A.ToJSON (Paving constraint basicSet set) where
  toEncoding = A.genericToEncoding A.defaultOptions

class CanPrune m constraint basicSet set where
  pruneProblemM :: Problem constraint basicSet -> m (Paving constraint basicSet set)

showPavingSummary :: (IsSet set) => Paving constraint basicSet set -> String
showPavingSummary (Paving {..}) =
  printf "{inner: %s, undecided: %s, outer: %s}" (showStats inner) statsUndecided (showStats outer)
    where
      statsUndecided = printf "|problems to do| = %d" (length undecided) :: String

-- printf "{inner: ??, undecided: %s, outer: %s}" (setShowStats undecided) (setShowStats outer)
-- disabling printing of "inner" to avoid inconsistent slow-down when running with 1 thread

emptyPaving :: (IsSet set) => Paving constraint basicSet set
emptyPaving =
  Paving
    { inner = emptySet,
      undecided = [],
      outer = emptySet
    }

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

pavingInner :: (IsSet set) => set -> Paving constraint basicSet set
pavingInner inner = Paving {inner, undecided = [], outer = emptySet}

pavingOuter :: (IsSet set) => set -> Paving constraint basicSet set
pavingOuter outer = Paving {inner = emptySet, undecided = [], outer}

pavingUndecided :: (IsSet set) => [Problem constraint basicSet] -> Paving constraint basicSet set
pavingUndecided undecided = Paving {undecided, inner = emptySet, outer = emptySet}

pavingInnerOuter :: set -> set -> Paving constraint basicSet set
pavingInnerOuter inner outer = Paving {inner, undecided = [], outer}

pavingInnerUndecided :: (IsSet set) => set -> [Problem constraint basicSet] -> Paving constraint basicSet set
pavingInnerUndecided inner undecided = Paving {inner, undecided, outer = emptySet}

pavingOuterUndecided :: (IsSet set) => set -> [Problem constraint basicSet] -> Paving constraint basicSet set
pavingOuterUndecided outer undecided = Paving {inner = emptySet, undecided, outer}
