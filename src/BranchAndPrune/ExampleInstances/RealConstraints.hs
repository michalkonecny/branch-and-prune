{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

exprNeg :: (CanNegSameType r) => Expr s r -> Expr s r
exprNeg (Expr eval1 desc1) =
  Expr {eval = negate . eval1, description = printf "-(%s)" desc1}

exprPlus :: (CanAddSameType r) => Expr s r -> Expr s r -> Expr s r
exprPlus (Expr eval1 desc1) (Expr eval2 desc2) =
  Expr {eval = \b -> eval1 b + eval2 b, description = printf "(%s) + (%s)" desc1 desc2}

exprTimes :: (CanMulSameType r) => Expr s r -> Expr s r -> Expr s r
exprTimes (Expr eval1 desc1) (Expr eval2 desc2) =
  Expr {eval = \b -> eval1 b * eval2 b, description = printf "(%s)*(%s)" desc1 desc2}

-- Instances to conveniently build expressions using the usual numerical operators

instance (CanNegSameType r) => CanNeg (Expr s r) where
  type NegType (Expr s r) = Expr s r
  negate = exprNeg

instance (CanAddSameType r) => CanAddAsymmetric (Expr s r) (Expr s r) where
  type AddType (Expr s r) (Expr s r) = (Expr s r)
  add = exprPlus

instance (CanAddSameType r, CanGetLiteral s r) => CanAddAsymmetric (Expr s r) Rational where
  type AddType (Expr s r) Rational = (Expr s r)
  add e q = exprPlus e (exprLit q)

instance (CanAddSameType r, CanGetLiteral s r) => CanAddAsymmetric Rational (Expr s r) where
  type AddType Rational (Expr s r) = (Expr s r)
  add q = exprPlus (exprLit q)

instance (CanNegSameType r, CanAddSameType r) => CanSub (Expr s r) (Expr s r)

instance (CanAddSameType r, CanGetLiteral s r) => CanSub (Expr s r) Rational

instance (CanNegSameType r, CanAddSameType r, CanGetLiteral s r) => CanSub Rational (Expr s r)

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

formLeq :: Expr s r -> Expr s r -> Form (Expr s r)
formLeq = FormComp CompLeq

formNeg :: Form expr -> Form expr
formNeg = FormUnary ConnNeg

formAnd :: Form expr -> Form expr -> Form expr
formAnd = FormBinary ConnAnd

formOr :: Form expr -> Form expr -> Form expr
formOr = FormBinary ConnOr

formImpl :: Form expr -> Form expr -> Form expr
formImpl = FormBinary ConnImpl

instance CanNeg (Form expr) where
  negate = formNeg

instance CanAndOrAsymmetric (Form expr) (Form expr) where
  type AndOrType (Form expr) (Form expr) = (Form expr)
  and2 = formAnd
  or2 = formOr

instance ConvertibleExactly Bool (Form expr) where
  safeConvertExactly True = Right FormTrue
  safeConvertExactly False = Right FormFalse

instance HasOrderAsymmetric (Expr s r) (Expr s r) where
  type OrderCompareType (Expr s r) (Expr s r) = Form (Expr s r)
  leq = formLeq
  lessThan = undefined

instance (CanGetLiteral s r) => HasOrderAsymmetric (Expr s r) Rational where
  type OrderCompareType (Expr s r) Rational = Form (Expr s r)
  leq (e :: e) q = formLeq e ((exprLit q) :: e)
  lessThan = undefined

instance (CanGetLiteral s r) => HasOrderAsymmetric Rational (Expr s r) where
  type OrderCompareType Rational (Expr s r) = Form (Expr s r)
  leq q (e :: e) = formLeq ((exprLit q) :: e) e
  lessThan = undefined
