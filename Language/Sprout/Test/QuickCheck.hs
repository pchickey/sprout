
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sprout.Test.QuickCheck where

import Data.Word
import Data.Int
import Language.Sprout
import Language.Sprout.Machine

import Test.QuickCheck

newtype Binop a = Binop (E a -> E a -> E a)

instance Arbitrary (Binop Int32) where
  arbitrary = elements [ Binop Add, Binop Sub, Binop Mul, Binop Div ]

instance Arbitrary (Binop Word32) where
  arbitrary = elements [ Binop Add, Binop Sub, Binop Mul, Binop Div ]

instance Arbitrary (E Bool) where
  arbitrary = oneof
    [ do i <- arbitrary
         return (ConstE (i :: Bool))
    , do a <- arbitrary
         b <- arbitrary
         return (And a b)
    , do a <- arbitrary
         return (Not a)
    , do a <- arbitrary
         b <- arbitrary
         c <- arbitrary
         return (Mux (a :: E Bool) (b :: E Bool) (c :: E Bool ))
    , do a <- arbitrary
         b <- arbitrary
         return (Eq (a :: E Bool) (b :: E Bool ))
    , do a <- arbitrary
         b <- arbitrary
         return (Eq (a :: E Word32) (b :: E Word32))
    , do a <- arbitrary
         b <- arbitrary
         return (Eq (a :: E Int32) (b :: E Int32))
    , do a <- arbitrary
         b <- arbitrary
         return (Lt (a :: E Word32) (b :: E Word32))
    , do a <- arbitrary
         b <- arbitrary
         return (Lt (a :: E Int32) (b :: E Int32))
    ]

instance Arbitrary (E Int32) where
  arbitrary = oneof
    [ do i <- arbitrary
         return (ConstE (i :: Int32))
    , do a <- arbitrary
         b <- arbitrary
         op <- arbitrary
         case op of
           Binop o ->
             return (o (a :: E Int32) (b :: E Int32))
    , do a <- arbitrary
         b <- arbitrary
         c <- arbitrary
         return (Mux (a :: E Bool) (b :: E Int32) (c :: E Int32))
    ]

instance Arbitrary (E Word32) where
  arbitrary = oneof
    [ do i <- arbitrary
         return (ConstE (i :: Word32))
    , do a <- arbitrary
         b <- arbitrary
         op <- arbitrary
         case op of
           Binop o ->
             return (o (a :: E Word32) (b :: E Word32))
    , do a <- arbitrary
         b <- arbitrary
         c <- arbitrary
         return (Mux (a :: E Bool) (b :: E Word32) (c :: E Word32))
    ]
