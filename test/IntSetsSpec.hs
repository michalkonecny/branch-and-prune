{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntSetsSpec (spec) where

import qualified BranchAndPrune as BP
import Control.Monad.Logger (LoggingT (LoggingT), runStdoutLoggingT)
import IntSets
  ( BasicIntSet (BasicIntSet),
    IntConstraint (IntEq, IntFalse),
    IntSet (IntSet),
    IntSetStack (IntSetStack),
    intSetBranchAndPrune,
    intSetN, IntSetBPParams (..),
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "branch and prune over IntSet" $ do
    it "solves (=1) over scope {1, 2}" $
      do
        runStdoutLoggingT
          ( intSetBranchAndPrune
              ( IntSetBPParams
                  { scope = BasicIntSet 1 2,
                    constraint = IntEq 1
                  }
              )
              :: LoggingT IO (BP.Paving IntSet)
          )
          `shouldReturn` (BP.pavingInnerOuter (intSetN 1) (intSetN 2) :: BP.Paving IntSet)
