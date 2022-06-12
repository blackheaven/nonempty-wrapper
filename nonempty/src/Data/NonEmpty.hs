{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module        : Data.NonEmpty
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- Create NonEmpty version of any container.
module Data.NonEmpty
  ( -- * Base type
    NonEmpty,
    getNonEmpty,
    trustedNonEmpty,

    -- * Singleton constructor
    NonEmptySingleton (..),
    singleton,
    MkNonEmptySingletonApplicative (..),

    -- * From container
    NonEmptyFromContainer (..),
    nonEmpty,
    MkNonEmptyFromContainerFoldable (..),

    -- * Operations
    (<|),
    (|>),
  )
where

import Data.Kind
import Data.Proxy

-- | NonEmpty proofed value.
newtype NonEmpty a = NonEmpty
  { -- | Extract the NonEmpty proven value
    getNonEmpty :: a
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup a => Semigroup (NonEmpty a) where
  NonEmpty x <> NonEmpty y = NonEmpty $ x <> y

-- * Operations

(<|) :: Semigroup a => NonEmpty a -> a -> NonEmpty a
NonEmpty ne <| n = NonEmpty $ ne <> n

infixr 6 <|

(|>) :: Semigroup a => a -> NonEmpty a -> NonEmpty a
n |> NonEmpty ne = NonEmpty $ n <> ne

infixr 6 |>

-- | Trusted value
trustedNonEmpty :: a -> NonEmpty a
trustedNonEmpty = NonEmpty

-- | Singleton constructible value
class NonEmptySingleton a where
  type NonEmptySingletonElement a :: Type
  nonEmptySingleton :: Proxy a -> NonEmptySingletonElement a -> a

-- | Build a 'NonEmpty' value from a singleton value
singleton :: NonEmptySingleton a => Proxy a -> NonEmptySingletonElement a -> NonEmpty a
singleton p = trustedNonEmpty . nonEmptySingleton p

-- | Build 'NonEmptySingleton' for 'Applicative' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance NonEmptySingleton [a] via (MkNonEmptySingletonApplicative [a])
newtype MkNonEmptySingletonApplicative a
  = MkNonEmptySingletonApplicative a

instance Applicative f => NonEmptySingleton (f a) where
  type NonEmptySingletonElement (f a) = a
  nonEmptySingleton _ = pure

-- * From container

-- | Used to attempt conversion from possibly empty to 'NonEmpty'.
class NonEmptyFromContainer a where
  isNonEmpty :: a -> Bool

-- | Attempt 'NonEmpty' proof
nonEmpty :: NonEmptyFromContainer a => a -> Maybe (NonEmpty a)
nonEmpty x =
  if isNonEmpty x
    then Just $ trustedNonEmpty x
    else Nothing

-- | Build 'MkNonEmptyFromContainerFoldable' for 'Foldable' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance NonEmptyFromContainer [a] via (MkNonEmptyFromContainerFoldable [a])
newtype MkNonEmptyFromContainerFoldable a
  = MkNonEmptyFromContainerFoldable a

instance Foldable f => NonEmptyFromContainer (f a) where
  isNonEmpty = not . null
