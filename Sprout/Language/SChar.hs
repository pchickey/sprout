
module Sprout.Language.SChar where

import Sprout.Language.Type
import Sprout.Language.Ref
import Sprout.Language.SBool
import qualified Sprout.Language.Syntax as AST

newtype SChar = SChar { getSChar :: AST.Expr }
char :: Char -> SChar
char  = wrapExpr . AST.ExpLit . AST.LitChar

instance SproutType SChar where
  sproutType _ = AST.TyChar

instance SproutVar SChar where
  wrapVar = wrapVarExpr
  unwrapExpr = getSChar

instance SproutExpr SChar where
  wrapExpr = SChar

instance SproutStore SChar
instance SproutEq    SChar
instance SproutOrd   SChar 
