{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Sprout.Language.Area where

import Sprout.Language.Proxy
import Sprout.Language.Type

-- | Type proxies for @Area@s
type AProxy a = Proxy (a :: Area)

-- | The kind of memory-area types.
data Area
  = forall a. Stored a
  -- ^ This is lifting for a *-kinded type
-- unimplemented:
  -- | Struct Symbol
  -- | Array Nat Area

instance SproutType a => SproutType (Stored a) where
  sproutType _ = sproutType ( Proxy :: Proxy a )
