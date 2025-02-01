{-# LANGUAGE OverloadedStrings #-}

module BranchAndPrune.ExampleInstances.SimpleBoxes.JSON () where

import AERN2.MP qualified as MP
import BranchAndPrune.ExampleInstances.RealConstraints (Expr (..), Form (..), BinaryComp, BinaryConn, UnaryConn)
import BranchAndPrune.ExampleInstances.SimpleBoxes.Boxes (Box (..), Boxes (..))
import Data.Aeson (ToJSON (toJSON), (.=))
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

instance A.ToJSON (Expr b r) where
  toJSON e = A.toJSON e.description -- a dummy String JSON instance

instance A.ToJSON BinaryComp where
  toJSON bc = A.toJSON (show bc)

instance A.ToJSON UnaryConn where
  toJSON uc = A.toJSON (show uc)

instance A.ToJSON BinaryConn where
  toJSON bc = A.toJSON (show bc)

instance (A.ToJSON e) => A.ToJSON (Form e) where
  toEncoding = A.genericToEncoding A.defaultOptions

