{-# OPTIONS_GHC -Wno-partial-fields #-}

module BranchAndPrune.Steps
  ( Step (..), stepToJSON
  )
where

import BranchAndPrune.Sets (Paving (..))
import Data.Aeson qualified as A
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TE
import GHC.Generics

data Step basicSet set
  = InitStep
      { scope :: basicSet,
        constraint :: String
      }
  | PruneStep
      { scope :: basicSet,
        constraint :: String,
        prunedScope :: Paving set,
        prunedConstraint :: String
      }
  | SplitStep
      { scope :: basicSet,
        pieces :: [basicSet]
      }
  | GiveUpOnSetStep
      { scope :: basicSet,
        constraint :: String
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep
  deriving (Show, Generic)

deriving instance (Generic (Paving set))

instance (A.ToJSON set) => A.ToJSON (Paving set) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON basicSet, A.ToJSON set) => A.ToJSON (Step basicSet set) where
  toEncoding = A.genericToEncoding A.defaultOptions

stepToJSON :: (A.ToJSON basicSet, A.ToJSON set) => Step basicSet set -> String
stepToJSON step =
  case TE.decodeUtf8' $ A.encode step of
    Left err -> "Failed to decode UTF8: " ++ (show err)
    Right stepT -> T.unpack stepT
