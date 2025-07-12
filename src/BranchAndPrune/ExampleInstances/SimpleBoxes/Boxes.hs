{-# LANGUAGE RebindableSyntax #-}

module BranchAndPrune.ExampleInstances.SimpleBoxes.Boxes
  ( --
    BP.Problem (..),
    Box (..),
    mkBox,
    Boxes (..),
  )
where

import AERN2.MP (MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Ball (CentreRadius (CentreRadius))
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Affine.Type () -- has instance Hashable MPBall (TODO: move that instance)

import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ExampleInstances.SimpleBoxes.RealConstraints (Var)
import BranchAndPrune.Sets (Subset)
import Data.List (sortOn)
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

{- N-dimensional Boxes -}

data Box = Box {varDomains :: Map.Map Var MP.MPBall, splitOrder :: [Var]}

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

{- Collections of boxes. -}

data Boxes
  = Boxes [Box]
  | BoxesUnion [Boxes]
  deriving (P.Eq, Show)

boxesCount :: Boxes -> Integer
boxesCount (Boxes boxes) = length boxes
boxesCount (BoxesUnion union) = sum (map boxesCount union)

boxesAreaD :: Boxes -> Double
boxesAreaD (Boxes boxes) = sum (map boxAreaD boxes)
boxesAreaD (BoxesUnion union) = sum (map boxesAreaD union)

instance BP.ShowStats (Subset Boxes Box) where
  showStats (BP.Subset {..}) =
    printf "{|boxes| = %d, coverage = %3.4f%%}" (boxesCount subset) coveragePercent
    where
      coveragePercent = 100 * (boxesAreaD subset / boxAreaD superset)

instance BP.IsSet Boxes where
  emptySet = Boxes []
  setIsEmpty (Boxes boxes) = null boxes
  setIsEmpty (BoxesUnion union) = List.all BP.setIsEmpty union
  setUnion bs1 bs2 = BoxesUnion [bs1, bs2]

instance BP.BasicSetsToSet Box Boxes where
  basicSetsToSet = Boxes