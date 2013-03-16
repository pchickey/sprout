{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Sprout.Language.Proxy where

import GHC.TypeLits (Sing, fromSing, Symbol, Nat)

data Proxy a = Proxy

type SProxy a = Proxy (a :: *)

fromTypeSym :: Sing (sym :: Symbol) -> String
fromTypeSym  = fromSing

fromTypeNat :: Sing (i :: Nat) -> Integer
fromTypeNat  = fromSing

