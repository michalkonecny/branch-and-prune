{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import BranchAndPrune.ExampleInstances.SimpleBoxes
  ( BoxBPParams (..),
    Expr,
    boxBranchAndPrune,
    exprLit,
    exprSum,
    exprVar,
    formAnd,
    formImpl,
    formLeq,
    mkBox,
  )
import Control.Monad.IO.Unlift (MonadUnliftIO, MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, NoLoggingT (runNoLoggingT), runStdoutLoggingT)
import System.Environment (getArgs)
import BranchAndPrune.BranchAndPrune (showPavingSummary, Result (Result))

x, y, z :: Expr
x = exprVar "x"
y = exprVar "y"
z = exprVar "z"

processArgs :: IO (Rational, Integer, Bool)
processArgs = do
  [epsS, maxThreadsS, debugS] <- getArgs
  let eps = toRational (read epsS :: Double)
  let maxThreads = read maxThreadsS :: Integer
  let debug = (debugS == "debug")
  return (eps, maxThreads, debug)

{-|
Example run:
```
time branch-and-prune-example 0.005 4 a +RTS -N4
```
-}
main :: IO ()
main =
  processArgs >>= mainWithArgs
  where
    mainWithArgs (eps, maxThreads, debug) =
      if debug
        then runStdoutLoggingT task
        else runNoLoggingT task
      where
        scope = (mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0)), ("z", (0.0, 2.0))])

        constraint = ((((exprSum x (exprLit eps)) `formLeq` y) `formAnd` (y `formLeq` z)) `formImpl` (x `formLeq` z))

        task :: (MonadLogger m, MonadUnliftIO m) => m ()
        task = do
          (Result paving _ _) <-
            boxBranchAndPrune $
              BoxBPParams
                { maxThreads,
                  giveUpAccuracy = eps / 10,
                  scope,
                  constraint
                }
          liftIO $ putStrLn $ showPavingSummary paving
