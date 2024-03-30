{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

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
module SimpleBoxes
  ( Box (..),
    mkBox,
    boxGetVarDomain,
    Boxes (..),
    BoxStack (..),
    Expr (..),
    Form (..),
    UnaryConn (..),
    BinaryConn (..),
    BinaryComp (..),
    BoxBPParams (..),
    boxBranchAndPrune,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import qualified AERN2.MP as MP
import AERN2.MP.Ball.Type (fromMPBallEndpoints, mpBallEndpoints)
import qualified BranchAndPrune as BP
import Control.Monad.Logger (MonadLogger)
import qualified Data.Map as Map
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import qualified Prelude as P
import Data.List (sortOn)

{- N-dimensional Boxes -}

type Var = String

data Box = Box {varDomains :: Map.Map Var MP.MPBall, splitOrder :: [Var]} 
  deriving (Show)

instance P.Eq Box where
  b1 == b2 = varDomainsR b1 == varDomainsR b2

varDomainsR :: Box -> [(Var, (Rational, Rational))]
varDomainsR b = sortOn fst $ map fromBall (Map.toList b.varDomains)
  where
    fromBall (var, ball) = (var, (rational lR, rational uR))
      where
        (lR, uR) = MP.endpoints ball

mkBox :: [(Var, (Rational, Rational))] -> Box
mkBox varDomainsRational =
  Box
    { varDomains = Map.fromList (map toBall varDomainsRational),
      splitOrder = map fst varDomainsRational
    }
  where
    toBall (var, (lR, uR)) = (var, MP.mpBall (mR, rR))
      where
        mR = (lR + uR) / 2
        rR = (uR - lR) / 2


boxGetVarDomain :: Box -> Var -> MP.MPBall
boxGetVarDomain (Box {..}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> dom

newtype Boxes = Boxes [Box] deriving (P.Eq, Show)

instance BP.IsSet Boxes where
  emptySet = Boxes []
  setIsEmpty (Boxes boxes) = null boxes
  setUnion (Boxes boxes1) (Boxes boxes2) = Boxes (boxes1 ++ boxes2)

instance BP.SetFromBasic Box Boxes where
  fromBasicSets = Boxes

instance BP.CanSplitSet Box Boxes where
  splitSet (Boxes boxes) = case boxes of
    [box] -> splitBox box -- split since we should return at least two boxes if possible
    _ -> boxes -- no box or at least two boxes

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
    m = (l + u) / 2
    bL = fromMPBallEndpoints l m
    bU = fromMPBallEndpoints m u

{- Non-linear Constraints -}

data Expr = Expr {eval :: Box -> MPBall, description :: String}

instance Show Expr where
  show expr = expr.description

instance P.Eq Expr where
  expr1 == expr2 = expr1.description == expr2.description

-- data UnaryOp = OpNeg | OpAbs | OpExp | OpSine | OpIntPow Integer

-- data BinaryOp = OpPlus | OpTimes | OpDiv | OpMax | OpMin

-- data Exp
--   = ExpLit Rational
--   | ExpVar Var
--   | ExpUnary UnaryOp Exp
--   | ExpBinary BinaryOp Exp Exp

data BinaryComp = CompLeq
  deriving (P.Eq, Show)

data UnaryConn = ConnNeg
  deriving (P.Eq, Show)

data BinaryConn = ConnAnd | ConnOr
  deriving (P.Eq, Show)

data Form
  = FormComp BinaryComp Expr Expr
  | FormUnary UnaryConn Form
  | FormBinary BinaryConn Form Form
  | FormTrue
  | FormFalse
  deriving (P.Eq, Show)

instance (Applicative m) => BP.CanPruneM m Box Form Boxes where
  pruneBasicSetM c b = pure (cP, pavingP)
    where
      cP = simplifyOnBox b c
      pavingP = case cP of
        FormTrue -> BP.pavingInner bSet
        FormFalse -> BP.pavingOuter bSet
        _ -> BP.pavingUndecided bSet
      bSet = BP.fromBasicSets [b]

simplifyOnBox :: Box -> Form -> Form
simplifyOnBox box = simplify
  where
    simplify (FormComp CompLeq e1 e2) =
      case e1.eval box <= e2.eval box of
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
    simplify FormTrue = FormTrue
    simplify FormFalse = FormFalse

newtype BoxStack = BoxStack [(Box, Form)]

instance BP.IsPriorityQueue BoxStack (Box, Form) where
  singletonQueue e = BoxStack [e]
  queueToList (BoxStack list) = list
  queuePickNext (BoxStack []) = Nothing
  queuePickNext (BoxStack (e : es)) = Just (e, BoxStack es)
  queueAddMany (BoxStack es) new_es = BoxStack (new_es ++ es)

data BoxBPParams = BoxBPParams
  { scope :: Box,
    constraint :: Form
  }

boxBranchAndPrune :: (MonadLogger m) => BoxBPParams -> m (BP.Paving Boxes)
boxBranchAndPrune (BoxBPParams {..}) =
  BP.branchAndPruneM
    ( BP.ParamsM
        { BP.scope,
          BP.constraint,
          BP.goalReached = (\_ -> False) :: BP.Paving Boxes -> Bool,
          BP.shouldGiveUpOnSet = (\_ -> False) :: Boxes -> Bool,
          BP.dummyPriorityQueue,
          BP.dummyMaction = pure ()
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [(undefined :: Box, FormFalse)]
