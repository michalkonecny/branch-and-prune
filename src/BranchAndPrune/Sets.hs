module BranchAndPrune.Sets
  ( ShowStats (..),
    IsSet (..),
    BasicSetsToSet(..),
    Subset(..),
    Problem (..),
    CanSplitProblem (..),
  )
where

import Data.Aeson qualified as A
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

data Problem constraint basicSet = Problem
  { scope :: basicSet,
    constraint :: constraint
  }
  deriving (Show, Generic)

instance (A.ToJSON basicSet, A.ToJSON constraint) => A.ToJSON (Problem constraint basicSet) where
  toEncoding = A.genericToEncoding A.defaultOptions

class CanSplitProblem constraint basicSet where
  splitProblem :: Problem constraint basicSet -> [Problem constraint basicSet] -- at least two so that B&P makes progress
