module BranchAndPrune.Sets
  ( ShowStats (..),
    IsSet (..),
    CanSplitProblem (..),
    Problem (..),
  )
where

import GHC.Generics (Generic)

import Data.Aeson qualified as A

class ShowStats t where
  showStats :: t -> String

class (ShowStats set) => IsSet set where
  emptySet :: set
  setIsEmpty :: set -> Bool
  setUnion :: set -> set -> set

data Problem constraint basicSet = Problem
  { scope :: basicSet,
    constraint :: constraint
  }
  deriving (Show, Generic)

instance (A.ToJSON basicSet, A.ToJSON constraint) => A.ToJSON (Problem constraint basicSet) where
  toEncoding = A.genericToEncoding A.defaultOptions

class CanSplitProblem constraint basicSet where
  splitProblem :: Problem constraint basicSet -> [Problem constraint basicSet] -- at least two so that B&P makes progress

