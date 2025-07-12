{-# OPTIONS_GHC -Wno-partial-fields #-}

module BranchAndPrune.Steps
  ( Step (..),
  )
where

import GHC.Generics

data Step problem paving
  = InitStep
      { problem :: problem
      }
  | PruneStep
      { problem :: problem,
        prunePaving :: paving
      }
  | SplitStep
      { problem :: problem,
        pieces :: [problem]
      }
  | GiveUpOnProblemStep
      { problem :: problem
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep
  deriving (Show, Generic)
