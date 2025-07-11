{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module provides the ingredients needed to apply the branch and bound algorithm to
-- solve real constraints with very simple pruning based only on straightforward interval evaluation.
module BranchAndPrune.ExampleInstances.SimpleBoxes
  ( BP.Problem (..),
    Var,
    Box (..),
    BoxProblem,
    mkBox,
    Boxes (..),
    BoxStack (..),
    ExprB,
    FormB,
    BoxBPParams (..),
    boxBranchAndPrune,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Ball.Type (fromMPBallEndpoints, mpBallEndpoints)
import BranchAndPrune.BranchAndPrune (CanSplitProblem (splitProblem))
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ExampleInstances.RealConstraints
import BranchAndPrune.ExampleInstances.SimpleBoxes.Boxes
import BranchAndPrune.Steps qualified as BP
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Hashable (Hashable)
import Data.Map qualified as Map
import GHC.Records
import MixedTypesNumPrelude

-- import Prelude qualified as P

{- Problem splitting -}
instance (Hashable constraint) => BP.CanSplitProblem constraint Box where
  splitProblem (BP.Problem {scope, constraint}) =
    map (\box -> BP.mkProblem $ BP.Problem_ {scope = box, constraint}) $ splitBox scope

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

type BoxProblem r = BP.Problem (FormB r) Box

type BoxPaving r = BP.Paving (FormB r) Box Boxes

type BoxStep r = BP.Step (BoxProblem r) (BoxPaving r)

type BoxResult r = BP.Result (FormB r) Box Boxes

type HasKleenanComparison r =
  ( HasOrder r r,
    OrderCompareType r r ~ Kleenean,
    HasEq r r,
    EqCompareType r r ~ Kleenean
  )

instance (Applicative m, HasKleenanComparison r) => BP.CanPrune m (FormB r) Box Boxes where
  pruneProblemM (BP.Problem {scope, constraint}) = pure pavingP
    where
      cP = simplifyOnBox scope constraint
      pavingP = case cP of
        FormTrue -> BP.pavingInner scope (Boxes [scope])
        FormFalse -> BP.pavingOuter scope (Boxes [scope])
        _ -> BP.pavingUndecided scope [BP.mkProblem $ BP.Problem_ {scope, constraint = cP}]

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

newtype BoxStack r = BoxStack [BoxProblem r]

instance BP.IsPriorityQueue (BoxStack r) (BoxProblem r) where
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
  { problem :: BoxProblem r,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

shouldGiveUpOnBoxProblem :: Rational -> BoxProblem r -> Bool
shouldGiveUpOnBoxProblem giveUpAccuracy (BP.Problem {scope = Box {..}}) =
  all smallerThanPrec (Map.elems varDomains)
  where
    smallerThanPrec :: MPBall -> Bool
    smallerThanPrec ball = diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

boxBranchAndPrune ::
  ( MonadLogger m,
    MonadUnliftIO m,
    HasKleenanComparison r,
    BP.CanControlSteps m (BoxStep r)
  ) =>
  BoxBPParams r ->
  m (BoxResult r)
boxBranchAndPrune (BoxBPParams {..} :: BoxBPParams r) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBoxProblem giveUpAccuracy :: BoxProblem r -> Bool,
          BP.dummyPriorityQueue,
          BP.maxThreads,
          BP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack r
    dummyPriorityQueue = BoxStack [problem]
