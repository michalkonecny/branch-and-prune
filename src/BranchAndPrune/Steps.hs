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
      { problemHash :: String,
        prunePaving :: paving
      }
  | SplitStep
      { problemHash :: String,
        pieces :: [problem]
      }
  | GiveUpOnProblemStep
      { problemHash :: String
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep
  deriving (Show, Generic)
