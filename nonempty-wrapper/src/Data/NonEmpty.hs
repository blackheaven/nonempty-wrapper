{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module        : Data.NonEmpty
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
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
    overNonEmpty,
    overNonEmpty2,
    overNonEmpty3,
    overNonEmpty4,
    overNonEmpty5,
    fmapNonEmpty,
    withNonEmpty,
  )
where

import Data.Kind
import Data.Maybe (fromJust)
import Data.Proxy

-- | NonEmpty proofed value.
newtype NonEmpty a = NonEmpty
  { -- | Extract the NonEmpty proven value
    getNonEmpty :: a
  }
  deriving stock (Eq, Ord, Show)

instance (Semigroup a) => Semigroup (NonEmpty a) where
  NonEmpty x <> NonEmpty y = NonEmpty $ x <> y

-- * Operations

-- | Append empty container
(<|) :: (Semigroup a) => NonEmpty a -> a -> NonEmpty a
NonEmpty ne <| n = NonEmpty $ ne <> n
{-# INLINE (<|) #-}

infixr 6 <|

-- | Prepend empty container
(|>) :: (Semigroup a) => a -> NonEmpty a -> NonEmpty a
n |> NonEmpty ne = NonEmpty $ n <> ne
{-# INLINE (|>) #-}

infixr 6 |>

-- | Wrap and unwrap 'NonEmpty' (unsafe, be sure 'f' is size-conservative)
overNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
overNonEmpty f = trustedNonEmpty . f . getNonEmpty
{-# INLINE overNonEmpty #-}

-- | Wrap and unwrap 'NonEmpty' (unsafe, be sure 'f' is size-conservative)
overNonEmpty2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
overNonEmpty2 f a = trustedNonEmpty . f (getNonEmpty a) . getNonEmpty
{-# INLINE overNonEmpty2 #-}

-- | Wrap and unwrap 'NonEmpty' (unsafe, be sure 'f' is size-conservative)
overNonEmpty3 :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
overNonEmpty3 f a b = trustedNonEmpty . f (getNonEmpty a) (getNonEmpty b) . getNonEmpty
{-# INLINE overNonEmpty3 #-}

-- | Wrap and unwrap 'NonEmpty' (unsafe, be sure 'f' is size-conservative)
overNonEmpty4 :: (a -> b -> c -> d -> e) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d -> NonEmpty e
overNonEmpty4 f a b c = trustedNonEmpty . f (getNonEmpty a) (getNonEmpty b) (getNonEmpty c) . getNonEmpty
{-# INLINE overNonEmpty4 #-}

-- | Wrap and unwrap 'NonEmpty' (unsafe, be sure 'f' is size-conservative)
overNonEmpty5 :: (a -> b -> c -> d -> e -> f) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d -> NonEmpty e -> NonEmpty f
overNonEmpty5 f a b c d = trustedNonEmpty . f (getNonEmpty a) (getNonEmpty b) (getNonEmpty c) (getNonEmpty d) . getNonEmpty
{-# INLINE overNonEmpty5 #-}

-- | 'fmap' over a 'NonEmpty' container
fmapNonEmpty :: (Functor f) => (a -> b) -> NonEmpty (f a) -> NonEmpty (f b)
fmapNonEmpty f = overNonEmpty (fmap f)
{-# INLINE fmapNonEmpty #-}

-- | Apply an unsafe function over empty, which is safe over 'NonEmpty'
withNonEmpty :: (a -> Maybe b) -> NonEmpty a -> b
withNonEmpty f = fromJust . f . getNonEmpty
{-# INLINE withNonEmpty #-}

-- | Trusted value
trustedNonEmpty :: a -> NonEmpty a
trustedNonEmpty = NonEmpty
{-# INLINE trustedNonEmpty #-}

-- | Singleton constructible value
class NonEmptySingleton a where
  type NonEmptySingletonElement a :: Type
  nonEmptySingleton :: Proxy a -> NonEmptySingletonElement a -> a

-- | Build a 'NonEmpty' value from a singleton value
singleton :: (NonEmptySingleton a) => Proxy a -> NonEmptySingletonElement a -> NonEmpty a
singleton p = trustedNonEmpty . nonEmptySingleton p
{-# INLINE singleton #-}

-- | Build 'NonEmptySingleton' for 'Applicative' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance NonEmptySingleton [a] via (MkNonEmptySingletonApplicative [a])
newtype MkNonEmptySingletonApplicative a
  = MkNonEmptySingletonApplicative a

instance (Applicative f) => NonEmptySingleton (f a) where
  type NonEmptySingletonElement (f a) = a
  nonEmptySingleton _ = pure

-- * From container

-- | Used to attempt conversion from possibly empty to 'NonEmpty'.
class NonEmptyFromContainer a where
  isNonEmpty :: a -> Bool

-- | Attempt 'NonEmpty' proof
nonEmpty :: (NonEmptyFromContainer a) => a -> Maybe (NonEmpty a)
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

instance (Foldable f) => NonEmptyFromContainer (f a) where
  isNonEmpty = not . null
