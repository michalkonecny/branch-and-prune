{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module BranchAndPrune.ExampleInstances.RealConstraintEval.MPBall () where

import AERN2.MP (MPBall)
import qualified AERN2.MP as MP
import BranchAndPrune.ExampleInstances.RealConstraints
import BranchAndPrune.ExampleInstances.SimpleBoxes
import qualified Data.Map as Map
import Text.Printf (printf)

boxGetVarDomain :: Box -> Var -> MP.MPBall
boxGetVarDomain (Box {..}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> dom

instance CanGetLiteral Box MPBall where
  getLiteral box = MP.mpBallP (MP.getPrecision box)

instance CanGetVarDomain Box MPBall where
  getVarDomain = boxGetVarDomain
