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

instance (A.ToJSON problem, A.ToJSON paving) => A.ToJSON (Step problem paving) where
  toEncoding = A.genericToEncoding A.defaultOptions
