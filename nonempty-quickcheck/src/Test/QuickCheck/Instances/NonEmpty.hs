{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module        : Test.QuickCheck.Instances.NonEmpty
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- QuickCheck's 'Arbitrary' instance for 'NonEmpty'.
module Test.QuickCheck.Instances.NonEmpty
  ( Arbitrary (..),
  )
where

import Data.NonEmpty
import Data.Proxy
import Test.QuickCheck hiding (getNonEmpty)

instance
  ( Arbitrary a,
    Semigroup a,
    NonEmptySingleton a,
    NonEmptyFromContainer a,
    Arbitrary (NonEmptySingletonElement a)
  ) =>
  Arbitrary (NonEmpty a)
  where
  arbitrary =
    (<|)
      <$> fmap (singleton $ Proxy @a) arbitrary
      <*> arbitrary

  shrink xs =
    [ trustedNonEmpty xs'
      | xs' <- shrink $ getNonEmpty xs,
        isNonEmpty xs'
    ]
