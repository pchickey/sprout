
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Sprout.Test.QuickCheck where

import Data.Word
import Data.Int
import Language.Sprout
import Language.Sprout.Machine

import Test.QuickCheck

data Rep = forall t. Rep (E t)

instance Arbitrary Rep where
  arbitrary = oneof
    [ do i <- arbitrary
         return (Rep (ConstE (i :: Word32)))
    , do i <- arbitrary
         return (Rep (ConstE (i :: Int32)))
    , do i <- arbitrary
         return (Rep (ConstE (i :: Bool)))
    , do
        a <- arbitrary
        b <- arbitrary
        case a of
          Rep c ->
            case b of
              Rep d ->
                return (Rep (Add c d))
    ]

