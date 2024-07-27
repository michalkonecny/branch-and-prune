module BranchAndPrune.ExampleInstances.RealConstraintEval.MPBall () where

import AERN2.MP (MPBall)
import qualified AERN2.MP as MP
import BranchAndPrune.ExampleInstances.RealConstraints
import BranchAndPrune.ExampleInstances.SimpleBoxes
import qualified Data.Map as Map
import Text.Printf (printf)

boxGetVarDomain :: Box -> Var -> MPBall
boxGetVarDomain (Box {..}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> dom

instance CanGetLiteral Box MPBall where
  getLiteral sampleMPBall _box = MP.mpBallP (MP.getPrecision sampleMPBall)

instance CanGetVarDomain Box MPBall where
  getVarDomain _sampleMPBall = boxGetVarDomain
