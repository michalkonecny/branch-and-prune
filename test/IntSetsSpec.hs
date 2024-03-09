{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}


module IntSetsSpec (spec) where

import Test.Hspec
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

import qualified BranchAndPrune as BP
import IntSets
  ( BasicIntSet (BasicIntSet),
    IntConstraint (IntEq, IntFalse),
    IntSet, intSetN,
    IntSetStack (IntSetStack),
  )

dummyPriorityQueue :: IntSetStack
dummyPriorityQueue = IntSetStack [(BasicIntSet 0 0, IntFalse)]

spec :: Spec
spec = do
  describe "branch and prune over IntSet" $ do
    it "solves (=1) over scope {1, 2}" $
      do
        runStdoutLoggingT (BP.branchAndPruneM
          ( BP.ParamsM
              { BP.scope = BasicIntSet 1 2,
                BP.constraint = IntEq 1,
                BP.goalReached = (\_ -> False) :: BP.Paving IntSet -> Bool,
                BP.shouldGiveUpOnSet = (\_ -> False) :: IntSet -> Bool,
                BP.dummyPriorityQueue,
                BP.dummyMaction = pure () :: LoggingT IO ()
              }
          ))
          `shouldReturn` (BP.pavingInnerOuter (intSetN 1) (intSetN 2) :: BP.Paving IntSet)
