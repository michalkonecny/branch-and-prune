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
    formIfThenElse,
  )
where

import Data.Set as Set
import MixedTypesNumPrelude
import Text.Printf (printf)
import qualified Prelude as P

type Var = String

{- Non-linear Expressions -}

data Expr b r = Expr {eval :: b -> r, vars :: Set.Set Var, sampleR :: r, description :: String}

class CanGetVarDomain b r where
  getVarDomain :: r -> b -> Var -> r

exprVar :: (CanGetVarDomain b r) => r -> Var -> Expr b r
exprVar sampleR var =
  Expr
    { eval = \b -> getVarDomain sampleR b var,
      vars = Set.singleton var,
      sampleR,
      description = var
    }

class CanGetLiteral b r where
  getLiteral :: r -> b -> Rational -> r

exprLit :: (CanGetLiteral b r) => r -> Rational -> Expr b r
exprLit sampleR literal = Expr {eval, vars = Set.empty, sampleR, description = show (double literal)}
  where
    eval scope = getLiteral sampleR scope literal

instance Show (Expr b r) where
  show expr = expr.description

instance P.Eq (Expr b r) where
  expr1 == expr2 = expr1.description == expr2.description

exprNeg :: (CanNegSameType r) => Expr b r -> Expr b r
exprNeg e =
  e {eval = negate . e.eval, description = printf "-(%b)" e.description}

exprSqrt :: (CanSqrtSameType r) => Expr b r -> Expr b r
exprSqrt e =
  e {eval = sqrt . e.eval, description = printf "sqrt(%b)" e.description}

exprSin :: (CanSinCosSameType r) => Expr b r -> Expr b r
exprSin e =
  e {eval = sin . e.eval, description = printf "sin(%b)" e.description}

exprCos :: (CanSinCosSameType r) => Expr b r -> Expr b r
exprCos e =
  e {eval = cos . e.eval, description = printf "cos(%b)" e.description}

exprPlus :: (CanAddSameType r) => Expr b r -> Expr b r -> Expr b r
exprPlus e1 e2 =
  Expr
    { eval = \b -> e1.eval b + e2.eval b,
      vars = e1.vars `Set.union` e2.vars,
      sampleR = e1.sampleR,
      description = printf "(%b) + (%b)" e1.description e2.description
    }

exprTimes :: (CanMulSameType r) => Expr b r -> Expr b r -> Expr b r
exprTimes e1 e2 =
  Expr
    { eval = \b -> e1.eval b * e2.eval b,
      vars = e1.vars `Set.union` e2.vars,
      sampleR = e1.sampleR,
      description = printf "(%b)*(%b)" e1.description e2.description
    }

-- Instances to conveniently build expressions using the usual numerical operators

instance (CanNegSameType r) => CanNeg (Expr b r) where
  type NegType (Expr b r) = Expr b r
  negate = exprNeg

instance (CanSqrtSameType r) => CanSqrt (Expr b r) where
  type SqrtType (Expr b r) = Expr b r
  sqrt = exprSqrt

instance (CanSinCosSameType r) => CanSinCos (Expr b r) where
  type SinCosType (Expr b r) = Expr b r
  sin = exprSin
  cos = exprCos

instance (CanAddSameType r) => CanAddAsymmetric (Expr b r) (Expr b r) where
  type AddType (Expr b r) (Expr b r) = (Expr b r)
  add = exprPlus

instance (CanAddSameType r, CanGetLiteral b r) => CanAddAsymmetric (Expr b r) Rational where
  type AddType (Expr b r) Rational = (Expr b r)
  add e q = exprPlus e (exprLit e.sampleR q)

instance (CanAddSameType r, CanGetLiteral b r) => CanAddAsymmetric Rational (Expr b r) where
  type AddType Rational (Expr b r) = (Expr b r)
  add q e = exprPlus (exprLit e.sampleR q) e

instance (CanNegSameType r, CanAddSameType r) => CanSub (Expr b r) (Expr b r)

instance (CanAddSameType r, CanGetLiteral b r) => CanSub (Expr b r) Rational

instance (CanNegSameType r, CanAddSameType r, CanGetLiteral b r) => CanSub Rational (Expr b r)

instance (CanMulSameType r) => CanMulAsymmetric (Expr b r) (Expr b r) where
  type MulType (Expr b r) (Expr b r) = (Expr b r)
  mul = exprTimes

instance (CanMulSameType r, CanGetLiteral b r) => CanMulAsymmetric (Expr b r) Rational where
  type MulType (Expr b r) Rational = (Expr b r)
  mul e q = exprTimes e (exprLit e.sampleR q)

instance (CanMulSameType r, CanGetLiteral b r) => CanMulAsymmetric Rational (Expr b r) where
  type MulType Rational (Expr b r) = (Expr b r)
  mul q e = exprTimes (exprLit e.sampleR q) e

{- Simple formulas over comparisons of expressions -}

data BinaryComp = CompLeq | CompEq
  deriving (P.Eq)

instance Show BinaryComp where
  show CompLeq = "<="
  show CompEq = "=="

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
  | FormIfThenElse (Form expr) (Form expr) (Form expr)
  | FormTrue
  | FormFalse
  deriving (P.Eq)

instance (Show expr) => Show (Form expr) where
  show :: Form expr -> String
  show (FormComp comp l r) = printf "%b %b %b" (show l) (show comp) (show r)
  show (FormUnary op l) = printf "%b (%b)" (show op) (show l)
  show (FormBinary op l r) = printf "(%b) %b (%b)" (show l) (show op) (show r)
  show (FormIfThenElse c t f) = printf "if (%b) then (%b) else (%b)" (show c) (show t) (show f)
  show FormTrue = "True"
  show FormFalse = "False"

formLeq :: Expr b r -> Expr b r -> Form (Expr b r)
formLeq = FormComp CompLeq

formEq :: Expr b r -> Expr b r -> Form (Expr b r)
formEq = FormComp CompEq

formNeg :: Form expr -> Form expr
formNeg = FormUnary ConnNeg

formAnd :: Form expr -> Form expr -> Form expr
formAnd = FormBinary ConnAnd

formOr :: Form expr -> Form expr -> Form expr
formOr = FormBinary ConnOr

formImpl :: Form expr -> Form expr -> Form expr
formImpl = FormBinary ConnImpl

formIfThenElse :: Form expr -> Form expr -> Form expr -> Form expr
formIfThenElse = FormIfThenElse

instance CanNeg (Form expr) where
  negate = formNeg

instance CanAndOrAsymmetric (Form expr) (Form expr) where
  type AndOrType (Form expr) (Form expr) = (Form expr)
  and2 = formAnd
  or2 = formOr

instance HasIfThenElse (Form expr) (Form expr) where
  type IfThenElseType (Form expr) (Form expr) =  (Form expr)
  ifThenElse = formIfThenElse

instance ConvertibleExactly Bool (Form expr) where
  safeConvertExactly True = Right FormTrue
  safeConvertExactly False = Right FormFalse

instance HasOrderAsymmetric (Expr b r) (Expr b r) where
  type OrderCompareType (Expr b r) (Expr b r) = Form (Expr b r)
  leq = formLeq
  lessThan = undefined

instance (CanGetLiteral b r) => HasOrderAsymmetric (Expr b r) Rational where
  type OrderCompareType (Expr b r) Rational = Form (Expr b r)
  leq (e :: e) q = formLeq e (exprLit e.sampleR q :: e)
  lessThan = undefined

instance (CanGetLiteral b r) => HasOrderAsymmetric Rational (Expr b r) where
  type OrderCompareType Rational (Expr b r) = Form (Expr b r)
  leq q (e :: e) = formLeq (exprLit e.sampleR q :: e) e
  lessThan = undefined

instance HasEqAsymmetric (Expr b r) (Expr b r) where
  type EqCompareType (Expr b r) (Expr b r) = Form (Expr b r)
  equalTo = formEq

instance (CanGetLiteral b r) => HasEqAsymmetric (Expr b r) Rational where
  type EqCompareType (Expr b r) Rational = Form (Expr b r)
  equalTo (e :: e) q = formEq e (exprLit e.sampleR q :: e)

instance (CanGetLiteral b r) => HasEqAsymmetric Rational (Expr b r) where
  type EqCompareType Rational (Expr b r) = Form (Expr b r)
  equalTo q (e :: e) = formEq (exprLit e.sampleR q :: e) e
