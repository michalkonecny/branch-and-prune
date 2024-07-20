{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import AERN2.AffArith (MPAffine, MPAffineConfig (..), mpAffineFromBall)
import AERN2.MP (MPBall, mpBallP, Kleenean)
import qualified AERN2.MP as MP
import BranchAndPrune.BranchAndPrune (Result (Result), showPavingSummary)
import BranchAndPrune.ExampleInstances.RealConstraintEval.AffArith ()
import BranchAndPrune.ExampleInstances.RealConstraintEval.MPBall ()
import BranchAndPrune.ExampleInstances.RealConstraints
  ( CanGetLiteral,
    CanGetVarDomain,
    -- exprLit,
    exprVar,
    formImpl,
  )
import BranchAndPrune.ExampleInstances.SimpleBoxes
  ( Box (..),
    BoxBPParams (..),
    ExprB,
    FormB,
    boxBranchAndPrune,
    mkBox,
  )
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT (runNoLoggingT), runStdoutLoggingT)
import Data.Map as Map
import Data.Maybe (fromJust)
-- import GHC.Records
import MixedTypesNumPrelude
import System.Environment (getArgs)
-- import qualified Prelude as P

data Problem r = Problem
  { scope :: Box,
    constraint :: FormB r
  }

type ProblemR r = (CanGetVarDomain Box r, CanGetLiteral Box r, 
  CanAddSameType r, CanMulSameType r, CanNegSameType r, 
  HasOrderAsymmetric r r, OrderCompareType r r ~ Kleenean)

problems :: (ProblemR r) => r -> Rational -> Map.Map String (Problem r)
problems (sampleR :: r) eps =
  Map.fromList
    [ ( "transitivityEps",
        Problem
          { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0)), ("z", (0.0, 2.0))],
            constraint = (((x + eps) <= y) && (y <= z)) `formImpl` (x <= z)
          }
      ),
      ( "circleEps",
        Problem
          { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0))],
            constraint = (x * x - 2.0 * x * y + y * y <= 1.0) `formImpl` (x - y <= 1.0 + eps)
          }
      ),
      ( "quadraticReduction",
        Problem
          { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
            constraint = (2.0 * x * x - 4.0 * x + 2.0 + y <= (- 4.0) * (x - 1.0) + y + eps)
          }
      ),
      ( "cubicReduction",
        Problem
          { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
            constraint = (6.0 * x * x * x + x * x - 10.0 * x + 3.0 + y <= (x - 1.0) * (x - 4.5) + y + eps)
          }
      )
    ]
  where
    x = exprVar sampleR "x" :: ExprB r
    y = exprVar sampleR "y" :: ExprB r
    z = exprVar sampleR "z" :: ExprB r

sampleMPBall :: MPBall
sampleMPBall = mpBallP MP.defaultPrecision 0

sampleMPAffine :: MPAffine
sampleMPAffine = mpAffineFromBall _conf 0 $ mpBallP MP.defaultPrecision 0
  where
    _conf :: MPAffineConfig
    _conf = MPAffineConfig {maxTerms = int 5, precision = 100}

processArgs :: (ProblemR r) => r -> [String] -> (Problem r, Rational, Integer, Bool)
processArgs sampleR [probS, epsS, maxThreadsS, debugS] =
  (prob, eps, maxThreads, debug)
  where
    eps = toRational (read epsS :: Double)
    prob = fromJust $ Map.lookup probS (problems sampleR eps)
    maxThreads = read maxThreadsS :: Integer
    debug = debugS == "debug"
processArgs _ _ = error "failed to match parameters"

-- |
-- Example runs:
--
-- > time branch-and-prune-example intervalArith transitivityEps 0.005 4 a +RTS -N4
--
-- > time branch-and-prune-example affineArith circleEps 0.001 4 nodebug +RTS -N4
main :: IO ()
main = do
  (arith : args) <- getArgs
  case arith of
    "intervalArith" ->
      mainWithArgs $ processArgs sampleMPBall args
    "affineArith" ->
      mainWithArgs $ processArgs sampleMPAffine args
    _ ->
      error $ "unknown arithmetic: " ++ arith

mainWithArgs :: ProblemR r => (Problem r, Rational, Integer, Bool) -> IO ()
mainWithArgs (Problem {scope, constraint} :: Problem r, eps, maxThreads, debug) =
  if debug
    then runStdoutLoggingT task
    else runNoLoggingT task
  where
    task :: (MonadLogger m, MonadUnliftIO m) => m ()
    task = do
      (Result paving _ _) <-
        boxBranchAndPrune
          $ BoxBPParams
            { maxThreads,
              giveUpAccuracy = eps / 10,
              scope,
              constraint
            }
      liftIO $ putStrLn $ showPavingSummary paving
