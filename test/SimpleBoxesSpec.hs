{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleBoxesSpec (spec) where

import qualified AERN2.MP as MP
import qualified BranchAndPrune as BP
import Control.Monad.Logger (runStdoutLoggingT)
-- import qualified Prelude as P
import GHC.Records
import MixedTypesNumPrelude
import SimpleBoxes
  ( BinaryComp (..),
    BinaryConn (..),
    Box (..),
    BoxBPParams (..),
    Boxes (..),
    Expr (..),
    Form (..),
    UnaryConn (..),
    Var,
    boxBranchAndPrune,
    boxGetVarDomain,
    mkBox,
  )
import Test.Hspec
import Text.Printf (printf)

runBP :: Rational -> Box -> Form -> IO (BP.Paving Boxes)
runBP giveUpAccuracy scope constraint =
  do
    (BP.Result paving _) <-
      runStdoutLoggingT
        $ boxBranchAndPrune (BoxBPParams {scope, constraint, giveUpAccuracy})
    pure paving

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

z :: Expr
z = exprVar "z"

exprVar :: Var -> Expr
exprVar var = Expr {eval = \b -> boxGetVarDomain b var, description = var}

exprLit :: Rational -> Expr
exprLit c = Expr {eval, description = show (double c)}
  where
    eval box = MP.mpBallP (MP.getPrecision box) c

exprSum :: Expr -> Expr -> Expr
exprSum (Expr eval1 desc1) (Expr eval2 desc2) =
  Expr {eval = \b -> eval1 b + eval2 b, description = printf "(%s) + (%s)" desc1 desc2}

formLeq :: Expr -> Expr -> Form
formLeq = FormComp CompLeq

formAnd :: Form -> Form -> Form
formAnd = FormBinary ConnAnd

formOr :: Form -> Form -> Form
formOr = FormBinary ConnOr

formImpl :: Form -> Form -> Form
formImpl = FormBinary ConnImpl

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
          `shouldReturn` ( BP.Paving
                             { inner = (BP.fromBasicSets [mkBox [("x", (0.0, 1.0))]]),
                               outer = (BP.fromBasicSets [mkBox [("x", (-1.0, -0.5))]]),
                               undecided = (BP.fromBasicSets [mkBox [("x", (-0.5, -0.25))], mkBox [("x", (-0.25, -0.0))]])
                             }
                         )
    it "solves (x+1 <= y ==> x <= y) over scope {x: [0,2], y: [0,2]} with give-up accuracy 0.25"
      $ do
        runBP 0.25 (mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0))])
          (((exprSum x (exprLit 1.0)) `formLeq` y) `formImpl` (x `formLeq` y))
          >>= (`shouldSatisfy` ( \ paving -> null paving.outer.boxes && null paving.undecided.boxes ))
    it "solves (x+0.1 <= y /\\ y <= z ==> x <= z) over scope {x: [0,2], y: [0,2], z: [0,2]} with give-up accuracy 0.01"
      $ do
        runBP 0.01 (mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0)), ("z", (0.0, 2.0))])
          ((((exprSum x (exprLit 0.1)) `formLeq` y) `formAnd` (y `formLeq` z)) `formImpl` (x `formLeq` z))
          >>= (`shouldSatisfy` ( \ paving -> null paving.outer.boxes && null paving.undecided.boxes ))
