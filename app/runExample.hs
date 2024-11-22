{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import AERN2.MP (Kleenean, MPBall, mpBallP)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (MPAffine), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Result (Result), showPavingSummary)
import BranchAndPrune.ExampleInstances.RealConstraintEval.AffArith ()
import BranchAndPrune.ExampleInstances.RealConstraintEval.MPBall ()
import BranchAndPrune.ExampleInstances.RealConstraints
  ( CanGetLiteral,
    CanGetVarDomain,
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
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
-- import GHC.Records
import MixedTypesNumPrelude
import System.Environment (getArgs)

-- import qualified Prelude as P

data Problem r = Problem
  { scope :: Box,
    constraint :: FormB r
  }

type ProblemR r =
  ( CanGetVarDomain Box r,
    CanGetLiteral Box r,
    CanAddSameType r,
    CanMulSameType r,
    CanNegSameType r,
    CanSqrtSameType r,
    CanSinCosSameType r,
    HasOrderAsymmetric r r,
    OrderCompareType r r ~ Kleenean,
    HasEqAsymmetric r r,
    EqCompareType r r ~ Kleenean
  )

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
      ( "circleEpsSqrt",
        Problem
          { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0))],
            constraint = ((sqrt $ x * x - 2.0 * x * y + y * y) <= 1.0) `formImpl` (x - y <= 1.0 + eps)
          }
      ),
      ( "quadraticReduction",
        Problem
          { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
            constraint = 2.0 * x * x - 4.0 * x + 2.0 + y <= (-4.0) * (x - 1.0) + y
          }
      ),
      ( "cubicReduction",
        Problem
          { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
            constraint = 6.0 * x * x * x + x * x - 10.0 * x + 3.0 + y <= (x - 1.0) * (x - 4.5) + y + eps
          }
      ),
      ( "vcApproxSinLE",
        Problem
          { scope = mkBox [("r1", (-3819831 / 4194304, 7639661 / 8388608)), ("x", (-6851933 / 8388608, 6851933 / 8388608))],
            constraint =
              let t =
                    ( ( x
                          * ( ( ( ( ((-3350387 / 17179869184) * (x * x))
                                      + (4473217 / 536870912)
                                  )
                                    * (x * x)
                                )
                                  + (-349525 / 2097152)
                              )
                                * (x * x)
                            )
                      )
                        + x
                    )
               in ( if x <= 1 / 67108864 && -x <= 1 / 67108864
                      then r1 == x
                      else
                        (r1 <= t + (4498891 / 100000000000000))
                          && ((t - (4498891 / 100000000000000)) <= r1)
                  )
                    && (not ((r1 + (-1.0 * (sin x))) <= (58 * (1 / 1000000000)) + eps))
          }
      )
    ]
  where
    x = exprVar sampleR "x" :: ExprB r
    y = exprVar sampleR "y" :: ExprB r
    z = exprVar sampleR "z" :: ExprB r
    r1 = exprVar sampleR "r1" :: ExprB r

sampleMPBall :: MPBall
sampleMPBall = mpBallP (MP.prec 1000) 0

sampleMPAffine :: MPAffine
sampleMPAffine = MPAffine _conf (convertExactly 0) Map.empty
  where
    _conf :: MPAffineConfig
    _conf = MPAffineConfig {maxTerms = int 10, precision = 1000}

processArgs :: (ProblemR r) => r -> [String] -> (Problem r, Rational, Integer, Bool)
processArgs sampleR [probS, epsS, giveUpAccuracyS, maxThreadsS, debugS] =
  (prob, giveUpAccuracy, maxThreads, debug)
  where
    prob = fromJust $ Map.lookup probS (problems sampleR eps)
    eps = toRational (read epsS :: Double)
    giveUpAccuracy = toRational (read giveUpAccuracyS :: Double)
    maxThreads = read maxThreadsS :: Integer
    debug = debugS == "debug"
processArgs _ _ =
  error
    $ "Failed to match args.  Expected args: arithmetic problem eps giveUpAccuracy maxThreads debug"
    ++ "\n Available arithmetics: IA, AA"
    ++ "\n Available problems: "
    ++ (List.concat $ List.map ("\n" ++) problemNames)
  where
    problemNames = Map.keys $ problems sampleMPBall 0.0

-- |
-- Example runs:
--
-- > time branch-and-prune-example intervalArith transitivityEps 0.005 4 a +RTS -N4
--
-- > time branch-and-prune-example affineArith cubicReduction 0.001 4 nodebug +RTS -N4
main :: IO ()
main = do
  (arith : args) <- getArgs
  case arith of
    "IA" ->
      mainWithArgs $ processArgs sampleMPBall args
    "AA" ->
      mainWithArgs $ processArgs sampleMPAffine args
    _ ->
      error $ "unknown arithmetic: " ++ arith

mainWithArgs :: (ProblemR r) => (Problem r, Rational, Integer, Bool) -> IO ()
mainWithArgs (Problem {scope, constraint} :: Problem r, giveUpAccuracy, maxThreads, debug) =
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
              giveUpAccuracy = giveUpAccuracy,
              scope,
              constraint
            }
      liftIO $ putStrLn $ showPavingSummary paving
