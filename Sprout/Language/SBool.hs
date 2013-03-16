
module Sprout.Language.SBool where

import Sprout.Language.Type
import Sprout.Language.Ref

import qualified Sprout.Language.Syntax as AST

newtype SBool = SBool { getSBool :: AST.Expr }

instance SproutType SBool where
  sproutType _ = AST.TyBool

instance SproutVar SBool where
  wrapVar = wrapVarExpr
  unwrapExpr = getSBool

instance SproutExpr SBool where
  wrapExpr = SBool

instance SproutStore SBool
