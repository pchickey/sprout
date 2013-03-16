
module Sprout.Language.Syntax.Type where

data Type
  = TyVoid
  | TyInt IntSize
  | TyWord WordSize
  | TyBool
  | TyChar
  | TyRef Type
  | TyConstRef Type
  deriving (Show, Eq, Ord)


data IntSize
  = Int8
  | Int16
  | Int32
  deriving (Show, Eq, Ord)

data WordSize
  = Word8
  | Word16
  | Word32
  deriving (Show, Eq, Ord)

