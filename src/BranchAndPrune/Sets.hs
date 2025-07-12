module BranchAndPrune.Sets
  ( ShowStats (..),
    IsSet (..),
    BasicSetsToSet (..),
    Subset (..),
    Problem (..),
    CanSplitProblem (..),
  )
where

import GHC.Generics (Generic)

class IsSet set where
  emptySet :: set
  setIsEmpty :: set -> Bool
  setUnion :: set -> set -> set

class BasicSetsToSet basicSet set where
  basicSetsToSet :: [basicSet] -> set

data Subset subset superset = Subset
  { subset :: subset,
    superset :: superset
  }

class ShowStats t where
  showStats :: t -> String

-- | A constraint problem with a scope and constraint.
data Problem constraint basicSet = Problem
  { scope :: basicSet,
    constraint :: constraint
  }
  deriving (Show, Generic)

class CanSplitProblem constraint basicSet where
  splitProblem :: Problem constraint basicSet -> [Problem constraint basicSet] -- at least two so that B&P makes progress
