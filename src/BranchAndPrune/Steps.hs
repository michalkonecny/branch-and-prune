{-# OPTIONS_GHC -Wno-partial-fields #-}

module BranchAndPrune.Steps
  ( Step (..),
    getStepProblems,
    getStepPavings,
  )
where

import BranchAndPrune.Paving (Paving (..), Problem)
import GHC.Generics

data Step problem paving evalInfo
  = InitStep
      { problem :: problem
      }
  | ProgressStep
      { problem :: problem,
        progressPaving :: paving,
        evalInfo :: evalInfo
      }
  | GiveUpOnProblemStep
      { problem :: problem
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep
  deriving (Show, Generic)

getStepProblems ::
  ( problem ~ Problem constraint basicSet,
    paving ~ Paving constraint basicSet set
  ) =>
  Step problem paving evalInfo ->
  [problem]
getStepProblems step = case step of
  InitStep {problem} -> [problem]
  ProgressStep {problem, progressPaving} -> problem : progressPaving.undecided
  GiveUpOnProblemStep {problem} -> [problem]
  AbortStep {} -> []
  DoneStep {} -> []

getStepPavings :: Step problem paving evalInfo -> [paving]
getStepPavings step = case step of
  ProgressStep {progressPaving} -> [progressPaving]
  _ -> []
