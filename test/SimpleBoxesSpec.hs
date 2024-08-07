{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}

module SimpleBoxesSpec (spec) where

import qualified BranchAndPrune.BranchAndPrune as BP
-- import qualified Prelude as P

import BranchAndPrune.ExampleInstances.RealConstraints
  ( exprLit,
    exprVar,
    formImpl,
  )
import BranchAndPrune.ExampleInstances.SimpleBoxes
  ( Box,
    BoxBPParams (..),
    Boxes (..),
    ExprB,
    FormB,
    boxBranchAndPrune,
    mkBox,
  )
import Control.Monad.Logger (runStdoutLoggingT)
import GHC.Records
import MixedTypesNumPrelude
import Test.Hspec

runBP :: Rational -> Box -> FormB -> IO (BP.Paving Boxes)
runBP giveUpAccuracy scope constraint =
  do
    result <-
      runStdoutLoggingT
        $ boxBranchAndPrune (BoxBPParams {scope, constraint, giveUpAccuracy, maxThreads = 4})
    pure result.paving

x, y, z :: ExprB
x = exprVar "x"
y = exprVar "y"
z = exprVar "z"

spec :: Spec
spec = do
  describe "branch and prune over Boxes" $ do
    it "solves (0<=x) over scope {x: [1,2]}"
      $ do
        runBP 0.25 (mkBox [("x", (1.0, 2.0))]) (0.0 <= x)
          `shouldReturn` BP.pavingInner (BP.fromBasicSets [mkBox [("x", (1.0, 2.0))]])
    it "solves (0<=x) over scope {x: [-2,-1]}"
      $ do
        runBP 0.25 (mkBox [("x", (-2.0, -1.0))]) (0.0 <= x)
          `shouldReturn` BP.pavingOuter (BP.fromBasicSets [mkBox [("x", (-2.0, -1.0))]])
    it "solves (0<=x) over scope {x: [-1,-1]} with give-up accuracy 0.25"
      $ do
        runBP 0.25 (mkBox [("x", (-1.0, 1.0))]) (0.0 <= x)
          `shouldReturn` ( BP.Paving
                             { inner = BP.fromBasicSets [mkBox [("x", (0.0, 1.0))]],
                               outer = BP.fromBasicSets [mkBox [("x", (-1.0, -0.5))]],
                               undecided = BP.fromBasicSets [mkBox [("x", (-0.5, -0.25))], mkBox [("x", (-0.25, -0.0))]]
                             }
                         )
    it "solves (x+1 <= y ==> x <= y) over scope {x: [0,2], y: [0,2]} with give-up accuracy 0.25"
      $ do
        runBP
          0.25
          (mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0))])
          (((x + 1.0) <= y) `formImpl` (x <= y))
          >>= (`shouldSatisfy` (\paving -> null paving.outer.boxes && null paving.undecided.boxes))
    it "solves (x+0.01 <= y /\\ y <= z ==> x <= z) over scope {x: [0,2], y: [0,2], z: [0,2]} with give-up accuracy 0.001"
      $ do
        runBP
          0.001
          (mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0)), ("z", (0.0, 2.0))])
          ((((x + 0.01) <= y) && (y <= z)) `formImpl` (x <= z))
          >>= (`shouldSatisfy` (\paving -> null paving.outer.boxes && null paving.undecided.boxes))
