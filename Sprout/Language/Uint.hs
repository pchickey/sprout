module Sprout.Language.Uint where

import Sprout.Language.Type
import Sprout.Language.Ref
import Sprout.Language.SBool
import qualified Sprout.Language.Syntax as AST

import Data.Word (Word8, Word16, Word32)

-- Signed 8 bit integer --------------------------------------------------------

newtype Uint8 = Uint8 { getUint8 :: AST.Expr }

instance SproutType Uint8 where
  sproutType _ = AST.TyWord AST.Word8

instance SproutVar Uint8 where
  wrapVar = wrapVarExpr
  unwrapExpr = getUint8

instance SproutExpr Uint8 where
  wrapExpr = Uint8

instance Num Uint8 where
  (+) = exprBinop (+)
  (-) = exprBinop (-)
  (*) = exprBinop (*)
  abs = exprUnary abs
  signum = exprUnary signum
  negate = exprUnary negate
  fromInteger = Uint8 . fromInteger

instance Bounded Uint8 where
  minBound = 0
  maxBound = fromIntegral (maxBound :: Word8)

instance SproutStore Uint8
instance SproutEq    Uint8
instance SproutOrd   Uint8

-- Signed 16 bit integer -------------------------------------------------------

newtype Uint16 = Uint16 { getUint16 :: AST.Expr }

instance SproutType Uint16 where
  sproutType _ = AST.TyWord AST.Word16

instance SproutVar Uint16 where
  wrapVar = wrapVarExpr
  unwrapExpr = getUint16

instance SproutExpr Uint16 where
  wrapExpr = Uint16

instance Num Uint16 where
  (+) = exprBinop (+)
  (-) = exprBinop (-)
  (*) = exprBinop (*)
  abs = exprUnary abs
  signum = exprUnary signum
  negate = exprUnary negate
  fromInteger = Uint16 . fromInteger

instance Bounded Uint16 where
  minBound = 0
  maxBound = fromIntegral (maxBound :: Word16)

instance SproutStore Uint16
instance SproutEq    Uint16
instance SproutOrd   Uint16

-- Signed 32 bit integer -------------------------------------------------------

newtype Uint32 = Uint32 { getUint32 :: AST.Expr }

instance SproutType Uint32 where
  sproutType _ = AST.TyWord AST.Word32

instance SproutVar Uint32 where
  wrapVar = wrapVarExpr
  unwrapExpr = getUint32

instance SproutExpr Uint32 where
  wrapExpr = Uint32

instance Num Uint32 where
  (+) = exprBinop (+)
  (-) = exprBinop (-)
  (*) = exprBinop (*)
  abs = exprUnary abs
  signum = exprUnary signum
  negate = exprUnary negate
  fromInteger = Uint32 . fromInteger

instance Bounded Uint32 where
  minBound = 0
  maxBound = fromIntegral (maxBound :: Word32)

instance SproutStore Uint32
instance SproutEq    Uint32
instance SproutOrd   Uint32
