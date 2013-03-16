{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Sprout.Language.Type where

import Sprout.Language.Proxy
import qualified Sprout.Language.Syntax as AST

class SproutType (t :: a) where
  sproutType :: Proxy t -> AST.Type

instance SproutType () where
  sproutType _ = AST.TyVoid

class SproutType t => SproutVar t where
  wrapVar    :: AST.Var -> t
  unwrapExpr :: t -> AST.Expr

class SproutVar t => SproutExpr t where
  wrapExpr :: AST.Expr -> t


wrapVarExpr :: SproutExpr t => AST.Var -> t
wrapVarExpr  = wrapExpr . AST.ExpVar

