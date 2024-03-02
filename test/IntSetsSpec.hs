{-# LANGUAGE NamedFieldPuns #-}

module IntSetsSpec (spec) where

import qualified BranchAndPrune as BP
import IntSets
import Test.Hspec

dummyPriorityQueue :: [(BasicIntSet, IntConstraint)]
dummyPriorityQueue = [(BasicIntSet 0 0, IntFalse)]

spec :: Spec
spec = do
  describe "branch and prune over IntSet" $ do
    it "solves (=1) over scope {1, 2}" $
      do
        BP.branchAndPrune
          ( BP.Params
              { BP.scope = BasicIntSet 1 2,
                BP.constraint = IntEq 1,
                BP.goalReached = \_ -> False,
                BP.shouldGiveUpOnSet = \_ -> False,
                BP.dummyPriorityQueue
              }
          )
          `shouldBe` (BP.pavingInnerOuter (intSetN 1) (intSetN 2))
