
module Sprout.Language.Syntax.AST where

import Sprout.Language.Syntax.Type

type Block = [Stmt]

data Stmt
  = IfTE Expr Block Block
  -- | Return (Typed Expr)
  -- | ReturnVoid
  | Deref Type Var Expr
  | Store Type Var Expr
  | Assign Type Var Expr
  -- | Call Type (Maybe Var) Name [Typed Expr]
  deriving (Show, Eq, Ord)

data Var = VarName String
  deriving (Show, Eq, Ord)

data Expr
  = ExpVar Var
  | ExpLit Literal
  | ExpSafeCast Type Expr
  | ExpOp ExpOp [Expr]
  deriving (Show, Eq, Ord)

data Literal
  = LitInteger Integer
  | LitChar Char
  | LitBool Bool
  deriving (Show, Eq, Ord)

data ExpOp
  = ExpEq Type
  | ExpNeq Type

  | ExpGt Bool Type
  | ExpLt Bool Type

  | ExpNot
  | ExpAnd
  | ExpOr

  | ExpAdd
  | ExpSub
  | ExpMul
  | ExpNegate
  | ExpAbs
  | ExpSignum

  | ExpDiv
  | ExpMod
  | ExpRecip

  deriving (Show, Eq, Ord)

instance Num Expr where
  l + r         = ExpOp ExpAdd [l, r]
  l - r         = ExpOp ExpSub [l, r]
  l * r         = ExpOp ExpMul [l, r]
  abs e         = ExpOp ExpAbs [e]
  signum e      = ExpOp ExpSignum [e]
  negate e      = ExpOp ExpNegate [e]
  fromInteger i = ExpLit (LitInteger i)

instance Fractional Expr where
  l / r        = ExpOp ExpDiv [l, r]
  recip a      = ExpOp ExpRecip [a]
  fromRational = error "fromRational not implemented for Expr"
