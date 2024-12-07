module BranchAndPrune.Sets
  ( ShowStats (..),
    IsSet (..),
    BasicSetsToSet (..),
    Subset (..),
    Problem (..),
    Problem_(..),
    mkProblem,
    CanSplitProblem (..),
  )
where

import Data.Aeson qualified as A
import Data.Hashable (Hashable (hash))
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
--
-- Construct using
-- @
-- mkProblem (Problem_ { scope = ..., constraint = ..., contentHash = 0 })
-- @
data Problem constraint basicSet = Problem
  { scope :: basicSet,
    constraint :: constraint,
    contentHash :: Int
  }
  deriving (Show, Generic)

data Problem_ constraint basicSet = Problem_
  { scope :: basicSet,
    constraint :: constraint
  }
  deriving (Show, Generic)

mkProblem ::
  (Hashable basicSet, Hashable constraint) =>
  Problem_ constraint basicSet ->
  Problem constraint basicSet
mkProblem (Problem_ {scope, constraint}) =
  Problem {contentHash = hash (scope, constraint), ..}

instance (A.ToJSON basicSet, A.ToJSON constraint) => A.ToJSON (Problem constraint basicSet) where
  toEncoding = A.genericToEncoding A.defaultOptions

class CanSplitProblem constraint basicSet where
  splitProblem :: Problem constraint basicSet -> [Problem constraint basicSet] -- at least two so that B&P makes progress
