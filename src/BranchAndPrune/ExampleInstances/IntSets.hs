-- |
-- This module impements very simple constraints over sets of integers and
-- all the ingredients needed to apply the branch and bound algorithm to
-- solve such constraints.  The pruning method chosen is deliberately sub-optimal
-- so that branching is required in addition to pruning
-- to solve the constraints.
--
-- This instance of the branch and bound algorithm is intended for executing the
-- algorithm in a very simple concrete context,
-- chiefly for testing and educational purposes.
module BranchAndPrune.ExampleInstances.IntSets
  ( IntSet (..),
    intSet,
    intSetN,
    intSetLU,
    BasicIntSet (..),
    IntConstraint (..),
    IntSetStack (..),
    IntSetBPParams(..),
    intSetBranchAndPrune,
  )
where

import qualified BranchAndPrune.BranchAndPrune as BP
import Control.Monad.Logger (MonadLogger)
import qualified Data.Set as Set
import Control.Monad.IO.Unlift (MonadUnliftIO)
import BranchAndPrune.LogUtils (logDebugStr)

newtype IntSet = IntSet (Set.Set Int) deriving (Eq, Show)

instance BP.IsSet IntSet where
  emptySet = IntSet Set.empty
  setIsEmpty (IntSet set) = Set.null set
  setUnion (IntSet set1) (IntSet set2) = IntSet (Set.union set1 set2)
  setShowStats (IntSet set) = show (Set.size set)

data BasicIntSet = BasicIntSet {lb :: Int, ub :: Int} deriving (Eq, Show)

instance BP.SetFromBasic BasicIntSet IntSet where
  fromBasicSets basicSets =
    IntSet (Set.fromList (concatMap doOne basicSets))
    where
      doOne (BasicIntSet l u) = [l .. u]

instance BP.CanSplitSet BasicIntSet IntSet where
  splitSet (IntSet nSet) =
    splitIfOnlyOne basicSets
    where
      splitIfOnlyOne [BasicIntSet l u]
        | l < u =
            [BasicIntSet l m, BasicIntSet (m + 1) u]
        where
          m = (l + u) `div` 2
      splitIfOnlyOne bs = bs
      basicSets =
        case nAscendingList of
          [] -> []
          (n : ns) -> lookForIntervals n n ns []
        where
          nAscendingList = Set.toAscList nSet
          lookForIntervals l u [] prev = BasicIntSet l u : prev
          lookForIntervals l u (n : ns) prev
            | n == u + 1 = lookForIntervals l n ns prev
            | otherwise = lookForIntervals n n ns (BasicIntSet l u : prev)

data IntConstraint = IntEq Int | IntTrue | IntFalse -- \| IntNeq Int
  deriving (Eq, Show)

intSet :: [Int] -> IntSet
intSet ns = IntSet (Set.fromList ns)

intSetLU :: Int -> Int -> IntSet
intSetLU l u = IntSet (Set.fromList [l .. u])

intSetN :: Int -> IntSet
intSetN n = IntSet (Set.singleton n)

instance (Applicative m) => BP.CanPrune m BasicIntSet IntConstraint IntSet where
  pruneBasicSetM c b = pure $ pruneBasicSet c b

pruneBasicSet :: IntConstraint -> BasicIntSet -> (IntConstraint, BP.Paving IntSet)
pruneBasicSet c@(IntEq n) (BasicIntSet l u)
  | l == n && n == u =
      (c, BP.pavingInner (intSetN n))
  | l <= n && n <= u =
      (c, BP.pavingOuterUndecided (intSetLU l (n - 1)) (intSetLU n u)) -- deliberately sub-optimal pruning
  | l == u =
      (c, BP.pavingOuter (intSetLU l u))
  | otherwise -- n < l < u || l < u < n
    =
      (IntFalse, BP.pavingUndecided (intSetLU l u)) -- no pruning but simplifying the constraint
pruneBasicSet c@IntTrue (BasicIntSet l u) = (c, BP.pavingInner (intSetLU l u))
pruneBasicSet c@IntFalse (BasicIntSet l u) = (c, BP.pavingOuter (intSetLU l u))

newtype IntSetStack = IntSetStack [(BasicIntSet, IntConstraint)]

instance BP.IsPriorityQueue IntSetStack (BasicIntSet, IntConstraint) where
  singletonQueue e = IntSetStack [e]
  queueToList (IntSetStack list) = list
  queuePickNext (IntSetStack []) = Nothing
  queuePickNext (IntSetStack (e : es)) = Just (e, IntSetStack es)
  queueAddMany (IntSetStack es) new_es = IntSetStack (new_es ++ es)
  queueSplit (IntSetStack es)
    | splitPoint == 0 = Nothing
    | otherwise = Just (IntSetStack esL, IntSetStack esR)
    where
      splitPoint = length es `div` 2
      (esL, esR) = splitAt splitPoint es
  queueMerge (IntSetStack stackL) (IntSetStack stackR) = IntSetStack $ stackL ++ stackR

data IntSetBPParams = IntSetBPParams
  { scope :: BasicIntSet,
    constraint :: IntConstraint
  }

intSetBranchAndPrune :: (MonadLogger m, MonadUnliftIO m) => IntSetBPParams -> m (BP.Result IntSet IntSetStack)
intSetBranchAndPrune (IntSetBPParams {..}) =
  BP.branchAndPruneM
    ( BP.Params
        { BP.scope,
          BP.constraint,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpOnBasicSet = const False :: BasicIntSet -> Bool,
          BP.dummyPriorityQueue,
          BP.maxThreads = 2,
          BP.logString = logDebugStr,
          BP.logStep = \ _ -> pure () -- do nothing
        }
    )
  where
    dummyPriorityQueue :: IntSetStack
    dummyPriorityQueue = IntSetStack [(BasicIntSet 0 0, IntFalse)]
