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
    IntSetBPParams (..),
    intSetBranchAndPrune,
  )
where

import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.Logging
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Aeson qualified as A
import Data.Set qualified as Set
import GHC.Generics

newtype IntSet = IntSet (Set.Set Int) deriving (Eq, Show, Generic)

instance A.ToJSON IntSet

instance BP.ShowStats IntSet where
  showStats (IntSet set) = show (Set.size set)

instance BP.IsSet IntSet where
  emptySet = IntSet Set.empty
  setIsEmpty (IntSet set) = Set.null set
  setUnion (IntSet set1) (IntSet set2) = IntSet (Set.union set1 set2)

data BasicIntSet = BasicIntSet {lb :: Int, ub :: Int} deriving (Eq, Show, Generic)

instance A.ToJSON BasicIntSet

data IntConstraint = IntEq Int | IntTrue | IntFalse -- \| IntNeq Int
  deriving (Eq, Show, Generic)

instance A.ToJSON IntConstraint

instance BP.CanSplitProblem IntConstraint BasicIntSet where
  splitProblem (BP.Problem {scope = BasicIntSet l u, constraint}) =
    map (\bs -> BP.Problem {scope = bs, constraint}) basicSets
    where
      basicSets = [BasicIntSet l m, BasicIntSet (m + 1) u]
        where
          m = (l + u) `div` 2

intSet :: [Int] -> IntSet
intSet ns = IntSet (Set.fromList ns)

intSetLU :: Int -> Int -> IntSet
intSetLU l u = IntSet (Set.fromList [l .. u])

intSetN :: Int -> IntSet
intSetN n = IntSet (Set.singleton n)

instance (Applicative m) => BP.CanPrune m IntConstraint BasicIntSet IntSet where
  pruneProblemM (BP.Problem {scope, constraint}) = pure $ pruneBasicSet constraint scope

pruneBasicSet :: IntConstraint -> BasicIntSet -> BP.Paving IntConstraint BasicIntSet IntSet
pruneBasicSet constraint@(IntEq n) (BasicIntSet l u)
  | l == n && n == u =
      BP.pavingInner (intSetN n)
  | l <= n && n <= u =
      -- deliberately sub-optimal pruning
      BP.pavingOuterUndecided (intSetLU l (n - 1)) [BP.Problem {scope = BasicIntSet n u, constraint}]
  | l == u =
      BP.pavingOuter (intSetLU l u)
  | otherwise -- n < l < u || l < u < n
    =
      -- no pruning but simplifying the constraint
      BP.pavingUndecided [BP.Problem {scope = BasicIntSet l u, constraint = IntFalse}]
pruneBasicSet IntTrue (BasicIntSet l u) = BP.pavingInner (intSetLU l u)
pruneBasicSet IntFalse (BasicIntSet l u) = BP.pavingOuter (intSetLU l u)

newtype IntSetStack elem = IntSetStack [elem]

instance BP.IsPriorityQueue (IntSetStack elem) elem where
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
  { problem :: BP.Problem IntConstraint BasicIntSet
  }

intSetBranchAndPrune :: (MonadLogger m, MonadUnliftIO m) => IntSetBPParams -> m (BP.Result IntConstraint BasicIntSet IntSet)
intSetBranchAndPrune (IntSetBPParams {..}) =
  BP.branchAndPruneM
    (defaultBPLogConfig {shouldLogDebugMessages = True})
    ( BP.Params
        { BP.problem,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = const False,
          BP.dummyPriorityQueue,
          BP.maxThreads = 2
        }
    )
  where
    dummyPriorityQueue :: IntSetStack (BP.Problem IntConstraint BasicIntSet)
    dummyPriorityQueue = IntSetStack [problem]
