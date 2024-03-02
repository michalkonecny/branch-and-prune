{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

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
module IntSets
  ( IntSet (..),
    BasicIntSet (..),
    IntConstraint (..),
  )
where

import qualified BranchAndPrune as BP
import qualified Data.Set as Set

newtype IntSet = IntSet (Set.Set Int)

instance BP.IsSet IntSet where
  emptySet = IntSet Set.empty
  setIsEmpty (IntSet set) = Set.null set
  setUnion (IntSet set1) (IntSet set2) = IntSet (Set.union set1 set2)

data BasicIntSet = BasicIntSet {lb :: Int, ub :: Int}

instance BP.SetFromBasic BasicIntSet IntSet where
  fromBasicSets basicSets =
    IntSet (Set.fromList (concat (map doOne basicSets)))
    where
      doOne (BasicIntSet l u) = [l .. u]

instance BP.CanSplitSet BasicIntSet IntSet where
  splitSet (IntSet nSet) =
    case nAscendingList of
      [] -> []
      (n : ns) -> lookForIntervals n n ns []
    where
      nAscendingList = Set.toAscList nSet
      lookForIntervals l u [] prev = (BasicIntSet l u) : prev
      lookForIntervals l u (n : ns) prev
        | n == u + 1 = lookForIntervals l n ns prev
        | otherwise = lookForIntervals n n ns ((BasicIntSet l u) : prev)

data IntConstraint = IntEq Int | IntTrue | IntFalse -- \| IntNeq Int

intSetLU :: Int -> Int -> IntSet
intSetLU l u = IntSet (Set.fromList [l .. u])

intSetN :: Int -> IntSet
intSetN n = IntSet (Set.singleton n)

instance BP.CanPrune BasicIntSet IntConstraint IntSet where
  pruneBasicSet c@(IntEq n) (BasicIntSet l u)
    | l == n && n == u =
        (c, BP.pavingInner (intSetN n))
    | l <= n && n <= u =
        (c, BP.pavingOuterUndecided (intSetLU l (n - 1)) (intSetLU n u)) -- deliberately sub-optimal
    | otherwise =
        (c, BP.pavingOuterUndecided (intSetLU l n) (intSetLU (n + 1) u)) -- deliberately sub-optimal
  pruneBasicSet c@(IntTrue) (BasicIntSet l u) = (c, BP.pavingInner (intSetLU l u))
  pruneBasicSet c@(IntFalse) (BasicIntSet l u) = (c, BP.pavingOuter (intSetLU l u))

instance BP.IsPriorityQueue [(BasicIntSet, IntConstraint)] (BasicIntSet, IntConstraint) where
  singletonQueue e = [e]
  queueToList = id
  queuePickNext [] = Nothing
  queuePickNext (e : es) = Just (e, es)
  queueAddMany = (++)
