{-# OPTIONS_GHC -Wno-partial-fields #-}

module BranchAndPrune.Steps
  ( Step (..),
  )
where

import Data.Aeson qualified as A
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

instance (A.ToJSON problem, A.ToJSON paving) => A.ToJSON (Step problem paving) where
  toEncoding = A.genericToEncoding (A.defaultOptions)
