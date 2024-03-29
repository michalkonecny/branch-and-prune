{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module IntSetsSpec (spec) where

import qualified BranchAndPrune as BP
import Control.Monad.Logger (runStdoutLoggingT)
import IntSets
  ( BasicIntSet (BasicIntSet),
    IntConstraint (..),
    IntSet (..),
    intSet,
    intSetN,
    IntSetBPParams (..),
    intSetBranchAndPrune,
  )
import Test.Hspec

runBP :: BasicIntSet -> IntConstraint -> IO (BP.Paving IntSet)
runBP scope constraint =
  runStdoutLoggingT $
    intSetBranchAndPrune $
      IntSetBPParams {..}

spec :: Spec
spec = do
  describe "branch and prune over IntSet" $ do
    it "solves (=1) over scope {1, 2}" $
      do
        runBP (BasicIntSet 1 2) (IntEq 1)
          `shouldReturn` (BP.pavingInnerOuter (intSetN 1) (intSetN 2))
    it "solves (=2) over scope {1,2,3}" $
      do
        runBP (BasicIntSet 1 3) (IntEq 2)
          `shouldReturn` (BP.pavingInnerOuter (intSetN 2) (intSet [1, 3]))
    it "solves (=4) over scope {1,2,3}" $
      do
        runBP (BasicIntSet 1 3) (IntEq 4)
          `shouldReturn` (BP.pavingOuter (intSet [1..3]))
