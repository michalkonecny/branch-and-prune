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
    LogConfig (..),
    boxBranchAndPrune,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Ball (CentreRadius (CentreRadius))
import AERN2.MP.Ball.Type (fromMPBallEndpoints, mpBallEndpoints)
import AERN2.MP.Dyadic (dyadic)
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ExampleInstances.RealConstraints
import BranchAndPrune.LogUtils (logDebugStr)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Aeson qualified as Aeson
import Data.List (sortOn)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.String (IsString (..)) -- for OverloadedStrings
-- import Database.Redis qualified as Redis
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TE
import GHC.Generics
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

{- N-dimensional Boxes -}

data Box = Box {varDomains :: Map.Map Var MP.MPBall, splitOrder :: [Var]}

deriving instance (Generic Box)
instance Aeson.ToJSON Box where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON MP.MPBall where
  toJSON b = Aeson.object ["l" .= lD, "u" .= uD]
    where
      (l, u) = MP.endpoints b
      lD = double l
      uD = double u
      (.=) = (Aeson..=)

instance Show Box where
  show (Box {..}) =
    printf "[%s]" $ List.intercalate ", " $ map showVarDom $ Map.toList varDomains
    where
      showVarDom :: (Var, MPBall) -> String
      showVarDom (var, ball) = printf "%s ∈ [%s..%s]" var (show (double l)) (show (double u))
        where
          (l, u) = MP.endpoints ball

instance P.Eq Box where
  b1 == b2 = varDomainsR b1 == varDomainsR b2

varDomainsR :: Box -> [(Var, (Rational, Rational))]
varDomainsR b = sortOn fst $ map fromBall (Map.toList b.varDomains)
  where
    fromBall (var, ball) = (var, (rational lR, rational uR))
      where
        (lR, uR) = MP.endpoints ball

instance MP.HasPrecision Box where
  getPrecision (Box {}) = MP.defaultPrecision -- TODO : use precision from varDomains if possible

mkBox :: [(Var, (Rational, Rational))] -> Box
mkBox varDomainsRational =
  Box
    { varDomains = Map.fromList (map toBall varDomainsRational),
      splitOrder = map fst varDomainsRational
    }
  where
    toBall (var, (lR, uR)) = (var, MP.mpBall (CentreRadius mR rR))
      where
        mR = (lR + uR) / 2
        rR = (uR - lR) / 2

boxAreaD :: Box -> Double
boxAreaD (Box {..}) = product (map (double . dyadic . MP.radius) (Map.elems varDomains))

data Boxes
  = Boxes [Box]
  | BoxesUnion [Boxes]
  deriving (P.Eq, Show)

deriving instance (Generic Boxes)
instance Aeson.ToJSON Boxes where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions


boxesCount :: Boxes -> Integer
boxesCount (Boxes boxes) = length boxes
boxesCount (BoxesUnion union) = sum (map boxesCount union)

boxesAreaD :: Boxes -> Double
boxesAreaD (Boxes boxes) = sum (map boxAreaD boxes)
boxesAreaD (BoxesUnion union) = sum (map boxesAreaD union)

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
    logConfig :: LogConfig
  }

data LogConfig where
  DoNotLog :: LogConfig
  LogToConsole :: LogConfig
  LogStepsToConsole :: LogConfig
  LogStepsToFile :: FilePath -> LogConfig
  -- LogStepsToRedisStream :: String -> LogConfig
  deriving (P.Eq)

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
  BP.branchAndPruneM
    ( BP.Params
        { BP.scope,
          BP.constraint,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpOnBasicSet = shouldGiveUpOnBox giveUpAccuracy :: Box -> Bool,
          BP.dummyPriorityQueue,
          BP.maxThreads,
          BP.logString = logString,
          BP.logStep = logStep
        }
    )
  where
    dummyPriorityQueue :: BoxStack r
    dummyPriorityQueue = BoxStack [(undefined :: Box, FormFalse)]

    logString = case logConfig of
      LogToConsole -> logDebugStr
      _ -> const (pure ())

    logStep = case logConfig of
      LogStepsToConsole -> liftIO . putStrLn . stepToJSON
      LogStepsToFile filePath -> const (pure ())
      _ -> const (pure ())

    stepToJSON step =
      (replicate 100 '-') ++ "\n" ++
      case TE.decodeUtf8' $ Aeson.encode step of
        Left err -> "Failed to decode UTF8: " ++ (show err)
        Right stepT -> T.unpack stepT