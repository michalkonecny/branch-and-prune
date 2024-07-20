{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BranchAndPrune.BranchAndPrune (Result (Result), showPavingSummary)
import BranchAndPrune.ExampleInstances.RealConstraints
  ( exprLit,
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
import GHC.Records
import MixedTypesNumPrelude
import System.Environment (getArgs)
import qualified Prelude as P
import BranchAndPrune.ExampleInstances.RealConstraintEval.AffArith
import AERN2.MP (MPBall, mpBallP)
import qualified AERN2.MP as MP

import AERN2.AffArith (MPAffine, MPAffineConfig(..), mpAffineFromBall, mpAffine)


data Problem = Problem
  { scope :: Box,
    constraint :: FormB MPAffine
  }

problems :: Rational -> Map.Map String Problem
problems eps =
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
      )
    ]

sampleMPAffine :: MPAffine
sampleMPAffine = mpAffineFromBall _conf 0 $ mpBallP MP.defaultPrecision 0
  where
  _conf :: MPAffineConfig
  _conf = MPAffineConfig {maxTerms = int 4, precision = 100}


x, y, z :: ExprB MPAffine
x = exprVar sampleMPAffine "x"
y = exprVar sampleMPAffine "y"
z = exprVar sampleMPAffine "z"

processArgs :: IO (Problem, Rational, Integer, Bool)
processArgs = do
  [probS, epsS, maxThreadsS, debugS] <- getArgs
  let eps = toRational (read epsS :: Double)
  let prob = fromJust $ Map.lookup probS (problems eps)
  let maxThreads = read maxThreadsS :: Integer
  let debug = debugS == "debug"
  return (prob, eps, maxThreads, debug)

-- |
-- Example runs:
--
-- > time branch-and-prune-example transitivityEps 0.005 4 a +RTS -N4
--
-- > time branch-and-prune-example circleEps 0.001 4 nodebug +RTS -N4
main :: IO ()
main =
  processArgs >>= mainWithArgs
  where
    mainWithArgs :: (Problem, Rational, Integer, Bool) -> IO ()
    mainWithArgs (Problem {scope, constraint}, eps, maxThreads, debug) =
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
