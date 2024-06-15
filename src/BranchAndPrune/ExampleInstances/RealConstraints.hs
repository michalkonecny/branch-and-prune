{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BranchAndPrune.ExampleInstances.RealConstraints
  ( Var,
    Expr (..),
    exprVar,
    CanGetVarDomain (..),
    exprLit,
    CanGetLiteral (..),
    Form (..),
    UnaryConn (..),
    BinaryConn (..),
    BinaryComp (..),
    formLeq,
    formAnd,
    formOr,
    formImpl,
  )
where

import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import qualified Prelude as P

type Var = String

{- Non-linear Expressions -}

data Expr s r = Expr {eval :: s -> r, description :: String}

class CanGetVarDomain s r where
  getVarDomain :: s -> Var -> r

exprVar :: (CanGetVarDomain s r) => Var -> Expr s r
exprVar var = Expr {eval = (`getVarDomain` var), description = var}

class CanGetLiteral s r where
  getLiteral :: s -> Rational -> r

exprLit :: (CanGetLiteral s r) => Rational -> Expr s r
exprLit literal = Expr {eval, description = show (double literal)}
  where
    eval scope = getLiteral scope literal

instance Show (Expr s r) where
  show expr = expr.description

instance P.Eq (Expr s r) where
  expr1 == expr2 = expr1.description == expr2.description

exprPlus :: (CanAddSameType r) => Expr s r -> Expr s r -> Expr s r
exprPlus (Expr eval1 desc1) (Expr eval2 desc2) =
  Expr {eval = \b -> eval1 b + eval2 b, description = printf "(%s) + (%s)" desc1 desc2}

exprTimes :: (CanMulSameType r) => Expr s r -> Expr s r -> Expr s r
exprTimes (Expr eval1 desc1) (Expr eval2 desc2) =
  Expr {eval = \b -> eval1 b * eval2 b, description = printf "(%s)*(%s)" desc1 desc2}

-- Instances to conveniently build expressions using the usual numerical operators

instance (CanAddSameType r) => CanAddAsymmetric (Expr s r) (Expr s r) where
  type AddType (Expr s r) (Expr s r) = (Expr s r)
  add = exprPlus

instance (CanAddSameType r, CanGetLiteral s r) => CanAddAsymmetric (Expr s r) Rational where
  type AddType (Expr s r) Rational = (Expr s r)
  add e q = exprPlus e (exprLit q)

instance (CanAddSameType r, CanGetLiteral s r) => CanAddAsymmetric Rational (Expr s r) where
  type AddType Rational (Expr s r) = (Expr s r)
  add q = exprPlus (exprLit q)

instance (CanMulSameType r) => CanMulAsymmetric (Expr s r) (Expr s r) where
  type MulType (Expr s r) (Expr s r) = (Expr s r)
  mul = exprTimes

instance (CanMulSameType r, CanGetLiteral s r) => CanMulAsymmetric (Expr s r) Rational where
  type MulType (Expr s r) Rational = (Expr s r)
  mul e q = exprTimes e (exprLit q)

instance (CanMulSameType r, CanGetLiteral s r) => CanMulAsymmetric Rational (Expr s r) where
  type MulType Rational (Expr s r) = (Expr s r)
  mul q = exprTimes (exprLit q)

{- Simple formulas over comparisons of expressions -}

formLeq :: Expr s r -> Expr s r -> Form (Expr s r)
formLeq = FormComp CompLeq

formAnd :: Form (Expr s r) -> Form (Expr s r) -> Form (Expr s r)
formAnd = FormBinary ConnAnd

formOr :: Form (Expr s r) -> Form (Expr s r) -> Form (Expr s r)
formOr = FormBinary ConnOr

formImpl :: Form (Expr s r) -> Form (Expr s r) -> Form (Expr s r)
formImpl = FormBinary ConnImpl

data BinaryComp = CompLeq
  deriving (P.Eq)

instance Show BinaryComp where
  show CompLeq = "<="

data UnaryConn = ConnNeg
  deriving (P.Eq)

instance Show UnaryConn where
  show ConnNeg = "¬"

data BinaryConn = ConnAnd | ConnOr | ConnImpl
  deriving (P.Eq)

instance Show BinaryConn where
  show ConnAnd = "∧"
  show ConnOr = "∨"
  show ConnImpl = "⇒"

data Form expr
  = FormComp BinaryComp expr expr
  | FormUnary UnaryConn (Form expr)
  | FormBinary BinaryConn (Form expr) (Form expr)
  | FormTrue
  | FormFalse
  deriving (P.Eq)

instance (Show expr) => Show (Form expr) where
  show :: Form expr -> String
  show (FormComp comp l r) = printf "%s %s %s" (show l) (show comp) (show r)
  show (FormUnary op l) = printf "%s (%s)" (show op) (show l)
  show (FormBinary op l r) = printf "(%s) %s (%s)" (show l) (show op) (show r)
  show FormTrue = "True"
  show FormFalse = "False"
