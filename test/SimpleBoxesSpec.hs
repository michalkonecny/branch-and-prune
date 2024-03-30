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

runBP :: Box -> Form -> IO (BP.Paving Boxes)
runBP scope constraint =
  runStdoutLoggingT
    $ boxBranchAndPrune
    $ BoxBPParams {..}

x :: Expr
x = Expr {eval = \b -> boxGetVarDomain b "x", description = "x"}

exprLit :: Rational -> Expr
exprLit c = Expr {eval = \b -> MP.mpBall (c, 0), description = show c}

spec :: Spec
spec = do
  describe "branch and prune over IntSet" $ do
    it "solves (0<=x) over scope {x: [1,2]}"
      $ do
        runBP (mkBox [("x", (1.0, 2.0))]) (FormComp CompLeq (exprLit 0.0) x)
          `shouldReturn` (BP.pavingInner (BP.fromBasicSets [mkBox [("x", (1.0, 2.0))]]))
