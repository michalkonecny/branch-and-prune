{-# OPTIONS_GHC -Wno-partial-fields #-}

module BranchAndPrune.Steps
  ( Step (..),
  getStepProblems,
  getStepPavings)
where

import GHC.Generics

data Step problem paving
  = InitStep
      { problem :: problem
      }
  | ProgressStep
      { problem :: problem,
        progressPaving :: paving
      }
  | GiveUpOnProblemStep
      { problem :: problem
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep
  deriving (Show, Generic)

getStepProblems :: Step problem paving -> [problem]
getStepProblems step = case step of
  InitStep {problem} -> [problem]
  ProgressStep {problem} -> [problem]
  GiveUpOnProblemStep {problem} -> [problem]
  AbortStep {} -> []
  DoneStep {} -> []

getStepPavings :: Step problem paving -> [paving]
getStepPavings step = case step of
  ProgressStep {progressPaving} -> [progressPaving]
  _ -> []
