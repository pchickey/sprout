{-# LANGUAGE ScopedTypeVariables #-}

module Sprout.Language.SBool where

import Sprout.Language.Type
import Sprout.Language.Proxy
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

-- IFTE surface syntax ---------------------------------------------------------

-- ifte :: IBool -> Sprout r a -> Sprout r b -> Sprout r ()
-- ifte cmp t f = do
-- etc


-- (?) :: forall a . SproutExpr a => IBool -> (a,a) -> a
-- cond ? (t,f) = etc


-- Constants -------------------------------------------------------------------

true, false :: SBool
true  = wrapExpr (AST.ExpLit (AST.LitBool True))
false = wrapExpr (AST.ExpLit (AST.LitBool False))

-- Comparisons -----------------------------------------------------------------

boolOp :: forall a . SproutVar a => (AST.Type -> AST.ExpOp) -> a -> a -> SBool
boolOp op a b = wrapExpr (AST.ExpOp (op ty) [unwrapExpr a, unwrapExpr b])
  where
  ty = sproutType (Proxy :: Proxy a)

class SproutExpr a => SproutEq a where
  (==?) :: a -> a -> SBool
  (==?)  = boolOp AST.ExpEq
  infix 4 ==?

  (/=?) :: a -> a -> SBool
  (/=?)  = boolOp AST.ExpNeq
  infix 4 /=?

class SproutEq a => SproutOrd a where
  (>?)  :: a -> a -> SBool
  (>?)   = boolOp (AST.ExpGt False)
  infix 4 >?

  (>=?) :: a -> a -> SBool
  (>=?)  = boolOp (AST.ExpGt True)
  infix 4 >=?

  (<?)  :: a -> a -> SBool
  (<?)   = boolOp (AST.ExpLt False)
  infix 4 <?

  (<=?) :: a -> a -> SBool
  (<=?)  = boolOp (AST.ExpLt True)
  infix 4 <=?

instance SproutType area => SproutEq (Ref area)

instance SproutEq  SBool
instance SproutOrd SBool


-- Boolean Logic ---------------------------------------------------------------

sNot :: SBool -> SBool
sNot a = wrapExpr (AST.ExpOp AST.ExpNot [unwrapExpr a])

(.&&) :: SBool -> SBool -> SBool
l .&& r = wrapExpr (AST.ExpOp AST.ExpAnd [unwrapExpr l, unwrapExpr r])
infixr 3 .&&

(.||) :: SBool -> SBool -> SBool
l .|| r = wrapExpr (AST.ExpOp AST.ExpOr [unwrapExpr l, unwrapExpr r])
infixr 2 .||

