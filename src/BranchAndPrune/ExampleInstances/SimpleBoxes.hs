{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module provides the ingredients needed to apply the branch and bound algorithm to
-- solve real constraints with very simple pruning based only on straightforward interval evaluation.
module BranchAndPrune.ExampleInstances.SimpleBoxes
  ( Var,
    Box (..),
    mkBox,
    Boxes (..),
    BoxStack (..),
    ExprB,
    FormB,
    BoxBPParams (..),
    BPLogConfig (..),
    boxBranchAndPrune,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Ball.Type (fromMPBallEndpoints, mpBallEndpoints)
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ExampleInstances.SimpleBoxes.Boxes
import BranchAndPrune.ExampleInstances.SimpleBoxes.JSON ()
import BranchAndPrune.ExampleInstances.RealConstraints
import BranchAndPrune.Logging
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.String (IsString (..)) -- for OverloadedStrings
-- import Database.Redis qualified as Redis
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
-- import Prelude qualified as P

instance BP.IsSet Boxes where
  emptySet = Boxes []
  setIsEmpty (Boxes boxes) = null boxes
  setIsEmpty (BoxesUnion union) = List.all BP.setIsEmpty union
  setUnion bs1 bs2 = BoxesUnion [bs1, bs2]
  setShowStats bs =
    printf "{|boxes| = %d, area = %3.2f}" (boxesCount bs) (boxesAreaD bs)

instance BP.SetFromBasic Box Boxes where
  fromBasicSets = Boxes

instance BP.CanSplitSet Box Boxes where
  splitSet (Boxes boxes) = case boxes of
    [box] -> splitBox box -- split since we should return at least two boxes if possible
    _ -> boxes -- no box or at least two boxes
  splitSet (BoxesUnion union) = List.concatMap BP.splitSet union

splitBox :: Box -> [Box]
splitBox box = case box.splitOrder of
  [] -> [box] -- No split order?? Perhaps there are no variables that can be split, ie are exact points.
  (splitVar : splitRest) ->
    -- We split the first variable in the list.
    let splitOrder = splitRest ++ [splitVar] -- Cycle the variables round-robin-like.
     in case Map.lookup splitVar box.varDomains of
          Nothing -> [box] -- The split variable does not exist...
          Just splitVarDomain ->
            [ Box {varDomains = varDomainsL, splitOrder},
              Box {varDomains = varDomainsU, splitOrder}
            ]
            where
              (splitVarDomainL, splitVarDomainU) = splitMPBall splitVarDomain
              varDomainsL = Map.insert splitVar splitVarDomainL box.varDomains
              varDomainsU = Map.insert splitVar splitVarDomainU box.varDomains

splitMPBall :: MPBall -> (MPBall, MPBall)
splitMPBall b = (bL, bU)
  where
    (l, u) = mpBallEndpoints b
    m = (l + u) / 2 -- TODO: adjust precision if needed to get the exact middle
    bL = fromMPBallEndpoints l m
    bU = fromMPBallEndpoints m u

type ExprB r = Expr Box r

type FormB r = Form (ExprB r)

type HasKleenanComparison r =
  ( HasOrder r r,
    OrderCompareType r r ~ Kleenean,
    HasEq r r,
    EqCompareType r r ~ Kleenean
  )

instance (Applicative m, HasKleenanComparison r) => BP.CanPrune m Box (FormB r) Boxes where
  pruneBasicSetM c b = pure (cP, pavingP)
    where
      cP = simplifyOnBox b c
      pavingP = case cP of
        FormTrue -> BP.pavingInner bSet
        FormFalse -> BP.pavingOuter bSet
        _ -> BP.pavingUndecided bSet
      bSet = BP.fromBasicSets [b]

simplifyOnBox :: (HasKleenanComparison r) => Box -> FormB r -> FormB r
simplifyOnBox box = simplify
  where
    simplify :: (HasKleenanComparison r) => FormB r -> FormB r
    simplify (FormComp CompLeq e1 e2) =
      case e1.eval box <= e2.eval box of
        CertainTrue -> FormTrue
        CertainFalse -> FormFalse
        TrueOrFalse -> FormComp CompLeq e1 e2
    simplify (FormComp CompEq e1 e2) =
      case e1.eval box == e2.eval box of
        CertainTrue -> FormTrue
        CertainFalse -> FormFalse
        TrueOrFalse -> FormComp CompLeq e1 e2
    simplify (FormUnary ConnNeg f1) =
      case simplify f1 of
        FormTrue -> FormFalse
        FormFalse -> FormTrue
        simplifiedF1 -> FormUnary ConnNeg simplifiedF1
    simplify (FormBinary ConnAnd f1 f2) =
      case (simplify f1, simplify f2) of
        (FormFalse, _) -> FormFalse
        (_, FormFalse) -> FormFalse
        (FormTrue, simplifiedF2) -> simplifiedF2
        (simplifiedF1, FormTrue) -> simplifiedF1
        (simplifiedF1, simplifiedF2) -> FormBinary ConnAnd simplifiedF1 simplifiedF2
    simplify (FormBinary ConnOr f1 f2) =
      case (simplify f1, simplify f2) of
        (FormTrue, _) -> FormTrue
        (_, FormTrue) -> FormTrue
        (FormFalse, simplifiedF2) -> simplifiedF2
        (simplifiedF1, FormFalse) -> simplifiedF1
        (simplifiedF1, simplifiedF2) -> FormBinary ConnOr simplifiedF1 simplifiedF2
    simplify (FormBinary ConnImpl f1 f2) =
      case (simplify f1, simplify f2) of
        (FormFalse, _) -> FormTrue
        (_, FormTrue) -> FormTrue
        (FormTrue, simplifiedF2) -> simplifiedF2
        (simplifiedF1, FormFalse) -> FormUnary ConnNeg simplifiedF1
        (simplifiedF1, simplifiedF2) -> FormBinary ConnImpl simplifiedF1 simplifiedF2
    simplify (FormIfThenElse fc ft ff) =
      case (simplify fc, simplify ft, simplify ff) of
        (FormTrue, simplifiedT, _) -> simplifiedT
        (FormFalse, _, simplifiedF) -> simplifiedF
        (_, FormTrue, FormTrue) -> FormTrue
        (_, FormFalse, FormFalse) -> FormFalse
        (simplifiedC, simplifiedT, simplifiedF) -> FormIfThenElse simplifiedC simplifiedT simplifiedF
    simplify FormTrue = FormTrue
    simplify FormFalse = FormFalse

newtype BoxStack r = BoxStack [(Box, FormB r)]

instance BP.IsPriorityQueue (BoxStack r) (Box, FormB r) where
  singletonQueue e = BoxStack [e]
  queueToList (BoxStack list) = list
  queuePickNext (BoxStack []) = Nothing
  queuePickNext (BoxStack (e : es)) = Just (e, BoxStack es)
  queueAddMany (BoxStack es) new_es = BoxStack (new_es ++ es)
  queueSplit (BoxStack es)
    | splitPoint == 0 = Nothing
    | otherwise = Just (BoxStack esL, BoxStack esR)
    where
      splitPoint = length es `divI` 2
      (esL, esR) = splitAt splitPoint es

  queueMerge (BoxStack stackL) (BoxStack stackR) = BoxStack $ stackL ++ stackR

data BoxBPParams r = BoxBPParams
  { scope :: Box,
    constraint :: FormB r,
    maxThreads :: Integer,
    giveUpAccuracy :: Rational,
    logConfig :: BPLogConfig
  }

shouldGiveUpOnBox :: Rational -> Box -> Bool
shouldGiveUpOnBox giveUpAccuracy (Box {..}) =
  all smallerThanPrec (Map.elems varDomains)
  where
    smallerThanPrec :: MPBall -> Bool
    smallerThanPrec ball = diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

boxBranchAndPrune :: (MonadLogger m, MonadUnliftIO m, HasKleenanComparison r) => BoxBPParams r -> m (BP.Result Boxes (BoxStack r))
boxBranchAndPrune (BoxBPParams {..}) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  BP.branchAndPruneM logConfig
    ( BP.Params
        { BP.scope,
          BP.constraint,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpOnBasicSet = shouldGiveUpOnBox giveUpAccuracy :: Box -> Bool,
          BP.dummyPriorityQueue,
          BP.maxThreads
        }
    )
  where
    dummyPriorityQueue :: BoxStack r
    dummyPriorityQueue = BoxStack [(undefined :: Box, FormFalse)]
