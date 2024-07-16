{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module BranchAndPrune.ExampleInstances.RealConstraintEval.AffArith () where

import AERN2.AffArith (MPAffine)
import qualified AERN2.AffArith as Aff
import BranchAndPrune.ExampleInstances.RealConstraints
import BranchAndPrune.ExampleInstances.SimpleBoxes
import qualified Data.Map as Map
import Text.Printf (printf)

boxGetVarDomain :: MPAffine -> Box -> Var -> MPAffine
boxGetVarDomain sampleAff (Box {varDomains}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> Aff.mpAffineFromBall sampleAff.config errId dom
    where
      errId = var -- using variable's name as the error variable ID


instance CanGetLiteral Box MPAffine where
  getLiteral sampleAff _box = Aff.mpAffine sampleAff.config

instance CanGetVarDomain Box MPAffine where
  getVarDomain = boxGetVarDomain
