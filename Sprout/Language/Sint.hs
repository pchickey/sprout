module Sprout.Language.Sint where

import Sprout.Language.Type
import Sprout.Language.Ref
import Sprout.Language.SBool
import qualified Sprout.Language.Syntax as AST

import Data.Int (Int8, Int16, Int32)

-- Signed 8 bit integer --------------------------------------------------------

newtype Sint8 = Sint8 { getSint8 :: AST.Expr }

instance SproutType Sint8 where
  sproutType _ = AST.TyInt AST.Int8

instance SproutVar Sint8 where
  wrapVar = wrapVarExpr
  unwrapExpr = getSint8

instance SproutExpr Sint8 where
  wrapExpr = Sint8

instance Num Sint8 where
  (+) = exprBinop (+)
  (-) = exprBinop (-)
  (*) = exprBinop (*)
  abs = exprUnary abs
  signum = exprUnary signum
  negate = exprUnary negate
  fromInteger = Sint8 . fromInteger

instance Bounded Sint8 where
  minBound = fromIntegral (minBound :: Int8)
  maxBound = fromIntegral (maxBound :: Int8)

instance SproutStore Sint8
instance SproutEq    Sint8
instance SproutOrd   Sint8

-- Signed 16 bit integer -------------------------------------------------------

newtype Sint16 = Sint16 { getSint16 :: AST.Expr }

instance SproutType Sint16 where
  sproutType _ = AST.TyInt AST.Int16

instance SproutVar Sint16 where
  wrapVar = wrapVarExpr
  unwrapExpr = getSint16

instance SproutExpr Sint16 where
  wrapExpr = Sint16

instance Num Sint16 where
  (+) = exprBinop (+)
  (-) = exprBinop (-)
  (*) = exprBinop (*)
  abs = exprUnary abs
  signum = exprUnary signum
  negate = exprUnary negate
  fromInteger = Sint16 . fromInteger

instance Bounded Sint16 where
  minBound = fromIntegral (minBound :: Int16)
  maxBound = fromIntegral (maxBound :: Int16)

instance SproutStore Sint16
instance SproutEq    Sint16
instance SproutOrd   Sint16

-- Signed 32 bit integer -------------------------------------------------------

newtype Sint32 = Sint32 { getSint32 :: AST.Expr }

instance SproutType Sint32 where
  sproutType _ = AST.TyInt AST.Int32

instance SproutVar Sint32 where
  wrapVar = wrapVarExpr
  unwrapExpr = getSint32

instance SproutExpr Sint32 where
  wrapExpr = Sint32

instance Num Sint32 where
  (+) = exprBinop (+)
  (-) = exprBinop (-)
  (*) = exprBinop (*)
  abs = exprUnary abs
  signum = exprUnary signum
  negate = exprUnary negate
  fromInteger = Sint32 . fromInteger

instance Bounded Sint32 where
  minBound = fromIntegral (minBound :: Int32)
  maxBound = fromIntegral (maxBound :: Int32)

instance SproutStore Sint32
instance SproutEq    Sint32
instance SproutOrd   Sint32
