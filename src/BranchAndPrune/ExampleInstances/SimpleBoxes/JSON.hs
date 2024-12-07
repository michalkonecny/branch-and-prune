{-# LANGUAGE OverloadedStrings #-}

module BranchAndPrune.ExampleInstances.SimpleBoxes.JSON () where

import AERN2.MP qualified as MP

import BranchAndPrune.ExampleInstances.RealConstraints (Expr (..), Form (..))
import BranchAndPrune.ExampleInstances.SimpleBoxes.Boxes (Box (..), Boxes (..))
import Data.Aeson qualified as A
import GHC.Records ()
import MixedTypesNumPrelude

-- import Prelude qualified as P

{- JSON serialisation -}

instance A.ToJSON Boxes where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Box where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON MP.MPBall where
  toJSON b = A.object ["l" .= lD, "u" .= uD]
    where
      (l, u) = MP.endpoints b
      lD = double l
      uD = double u
      (.=) = (A..=)

instance A.ToJSON (Expr b r) where
  toJSON e = A.toJSON e.description -- a dummy String JSON instance

instance A.ToJSON (Form (Expr b r)) where
  toJSON = A.toJSON . show -- a dummy String JSON instance

