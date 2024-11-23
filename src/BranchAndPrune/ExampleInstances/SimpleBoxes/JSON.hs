{-# LANGUAGE OverloadedStrings #-}

module BranchAndPrune.ExampleInstances.SimpleBoxes.JSON () where

import AERN2.MP qualified as MP
import BranchAndPrune.ExampleInstances.SimpleBoxes.Boxes
import Data.Aeson qualified as Aeson
import GHC.Generics
-- import Database.Redis qualified as Redis
import MixedTypesNumPrelude
-- import Prelude qualified as P

{- JSON serialisation -}

deriving instance (Generic Boxes)

instance Aeson.ToJSON Boxes where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

deriving instance (Generic Box)

instance Aeson.ToJSON Box where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON MP.MPBall where
  toJSON b = Aeson.object ["l" .= lD, "u" .= uD]
    where
      (l, u) = MP.endpoints b
      lD = double l
      uD = double u
      (.=) = (Aeson..=)
