{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sprout.Language.Ref where

import Sprout.Language.Area
import Sprout.Language.Proxy
import Sprout.Language.Type
import qualified Sprout.Language.Syntax as AST

-- Things which are valid to store ---------------------------------------------

class SproutVar a => SproutStore a where

-- References ------------------------------------------------------------------

newtype Ref (a :: Area) = Ref { getRef :: AST.Expr }

instance SproutType area => SproutType (Ref area) where
  sproutType _ = AST.TyRef (sproutType (Proxy :: Proxy area))

instance SproutType area => SproutVar (Ref area) where
  wrapVar = wrapVarExpr
  unwrapExpr = getRef

instance SproutType area => SproutExpr (Ref area) where
  wrapExpr = Ref

-- Constant References ---------------------------------------------------------

newtype ConstRef (a :: Area) = ConstRef { getConstRef :: AST.Expr }

constRef :: SproutType area => Ref area -> ConstRef area
constRef  = wrapExpr . unwrapExpr

instance SproutType area => SproutType (ConstRef area) where
  sproutType _ = AST.TyConstRef (sproutType (Proxy :: Proxy area))

instance SproutType area => SproutVar (ConstRef area) where
  wrapVar = wrapVarExpr
  unwrapExpr = getConstRef

instance SproutType area => SproutExpr (ConstRef area) where
  wrapExpr = ConstRef

-- Dereferencing ---------------------------------------------------------------

-- deref :: reftype -> Sprout r a
-- deref ref = do blah

-- Storing ---------------------------------------------------------------------

-- store :: reftype -> val -> Sprout r ()
-- store ref a = do blah

