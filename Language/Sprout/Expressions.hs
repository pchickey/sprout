{-# LANGUAGE GADTs, DeriveDataTypeable #-}

module Language.Sprout.Expressions where

import Data.Generics hiding (typeOf)

import Data.Int
import Data.Word

data Type
  = TBool
  | TInt32
  | TWord32
  deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)

data Const
  = CBool Bool
  | CInt32 Int32
  | CWord32 Word32
  deriving (Show, Eq, Ord, Data, Typeable)

data Variable
  = VBool   (V Bool)
  | VInt32  (V Int32)
  | VWord32 (V Word32)
  deriving (Show, Eq, Ord, Data, Typeable)

-- All untyped variables are external
data UV = UV String Type deriving (Show, Eq, Ord, Data, Typeable)
-- Typed variables 
data V a = V UV deriving (Show, Eq, Ord, Data, Typeable)

-- | Untyped expression
data Expression
  = EBool (E Bool)
  | EInt32 (E Int32)
  | EWord32 (E Word32)

-- | Typed expression
data E a where
  VRef   :: V a -> E a
  ConstE :: a -> E a
  -- Numeric expressions:
  Add    :: NumE a => E a -> E a -> E a
  Sub    :: NumE a => E a -> E a -> E a
  Mul    :: NumE a => E a -> E a -> E a
  Div    :: NumE a => E a -> E a -> E a
  -- Bool expressions:
  And    :: E Bool -> E Bool -> E Bool
  Not    :: E Bool -> E Bool
  -- Comparison expressions
  Eq     :: EqE a   => E a -> E a -> E Bool
  Lt     :: OrdE a  => E a -> E a -> E Bool
  -- Branching
  Mux    :: E Bool -> E a -> E a -> E a

---------------------------------------------------------------

-- Predicates on E:
class Eq a => Expr a where
  eType      :: E a -> Type
  constant   :: a   -> Const
  expression :: E a -> Expression
  variable   :: V a -> Variable

instance Expr Bool where
  eType _    = TBool
  constant   = CBool
  expression = EBool
  variable   = VBool

instance Expr Int32 where
  eType _    = TInt32
  constant   = CInt32
  expression = EInt32
  variable   = VInt32

instance Expr Word32 where
  eType _    = TWord32
  constant   = CWord32
  expression = EWord32
  variable   = VWord32

class (Eq a, Expr a) => EqE a
instance EqE Bool
instance EqE Int32
instance EqE Word32

class (Ord a, Expr a) => OrdE a
instance OrdE Bool
instance OrdE Int32
instance OrdE Word32

class (Num a, Expr a, EqE a, OrdE a) => NumE a
instance NumE Int32
instance NumE Word32

---------------------------------------------------------------

-- | Untyped expressions
data UE
  = UVRef  UV
  | UConst Const
  | UAdd   UE UE
  | USub   UE UE
  | UMul   UE UE
  | UDiv   UE UE
  | UAnd   UE UE
  | UNot   UE
  | UEq    UE UE
  | ULt    UE UE
  | UMux   UE UE UE
  deriving (Eq, Show)

class TypeOf a where typeOf :: a -> Type

instance TypeOf Const where
  typeOf a = case a of
    CBool _    -> TBool
    CInt32  _  -> TInt32
    CWord32  _ -> TWord32

instance TypeOf UV where
  typeOf (UV _ t) = t

instance TypeOf UE where
  typeOf e = case e of
    UConst c   -> typeOf c
    UVRef  uv  -> typeOf uv
    UAdd a _   -> typeOf a
    USub a _   -> typeOf a
    UMul a _   -> typeOf a
    UDiv a _   -> typeOf a
    UAnd _ _   -> TBool
    UNot _     -> TBool
    UEq  _ _   -> TBool
    ULt  _ _   -> TBool
    UMux _ a _ -> typeOf a

-- | Converts a typed expression E a into an untyped expression UE
ue :: Expr a => E a -> UE
ue t = case t of
  VRef  (V v) -> UVRef v
  ConstE a    -> UConst $ constant a
  Add   a b   -> UAdd (ue a) (ue b)
  Sub   a b   -> USub (ue a) (ue b)
  Mul   a b   -> UMul (ue a) (ue b)
  Div   a b   -> UDiv (ue a) (ue b)
  And   a b   -> UAnd (ue a) (ue b)
  Not   a     -> UNot (ue a)
  Eq    a b   -> UEq  (ue a) (ue b)
  Lt    a b   -> ULt  (ue a) (ue b)
  Mux   a b c -> UMux (ue a) (ue b) (ue c)


