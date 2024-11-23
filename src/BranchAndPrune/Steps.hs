{-# OPTIONS_GHC -Wno-partial-fields #-}

module BranchAndPrune.Steps
  ( Step (..),
  )
where

import BranchAndPrune.Sets (Paving)

data Step basicSet set constraint
  = InitStep
      { scope :: basicSet,
        constraint :: constraint
      }
  | PruneStep
      { scope :: basicSet,
        constraint :: constraint,
        prunedScope :: Paving set,
        prunedConstraint :: constraint
      }
  | SplitStep
      { scope :: basicSet,
        pieces :: [basicSet]
      }
  | GiveUpOnSetStep
      { scope :: basicSet,
        constraint :: constraint
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep
