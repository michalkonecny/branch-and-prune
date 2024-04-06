{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleBoxesSpec (spec) where

import qualified BranchAndPrune as BP
import Control.Monad.Logger (runStdoutLoggingT)
import MixedTypesNumPrelude
import SimpleBoxes
  ( BinaryComp (..),
    BinaryConn (..),
    Box,
    BoxBPParams (..),
    Boxes (..),
    Expr (..),
    Form (..),
    UnaryConn (..),
    mkBox,
    boxBranchAndPrune,
    boxGetVarDomain,
  )
import Test.Hspec
import qualified Prelude as P
import qualified AERN2.MP as MP

runBP :: Rational -> Box -> Form -> IO (BP.Paving Boxes)
runBP giveUpAccuracy scope constraint =
  runStdoutLoggingT
    $ boxBranchAndPrune
    $ BoxBPParams {..}

x :: Expr
x = Expr {eval = \b -> boxGetVarDomain b "x", description = "x"}

exprLit :: Rational -> Expr
exprLit c = Expr {eval = \_ -> MP.mpBall (c, 0), description = show c}

spec :: Spec
spec = do
  describe "branch and prune over Boxes" $ do
    it "solves (0<=x) over scope {x: [1,2]}"
      $ do
        runBP 0.25 (mkBox [("x", (1.0, 2.0))]) (FormComp CompLeq (exprLit 0.0) x)
          `shouldReturn` (BP.pavingInner (BP.fromBasicSets [mkBox [("x", (1.0, 2.0))]]))
    it "solves (0<=x) over scope {x: [-2,-1]}"
      $ do
        runBP 0.25 (mkBox [("x", (-2.0, -1.0))]) (FormComp CompLeq (exprLit 0.0) x)
          `shouldReturn` (BP.pavingOuter (BP.fromBasicSets [mkBox [("x", (-2.0, -1.0))]]))
    it "solves (0<=x) over scope {x: [-1,-1]} with give-up accuracy 0.25"
      $ do
        runBP 0.25 (mkBox [("x", (-1.0, 1.0))]) (FormComp CompLeq (exprLit 0.0) x)
          `shouldReturn` (BP.Paving {
            inner = (BP.fromBasicSets [mkBox [("x", (0.0, 1.0))]]),
            outer = (BP.fromBasicSets [mkBox [("x", (-1.0, -0.5))], mkBox [("x", (-0.5, -0.25))]]),
            undecided = (BP.fromBasicSets [mkBox [("x", (-0.25, -0.0))]])
          })
