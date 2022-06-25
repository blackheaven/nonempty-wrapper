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
-- aeson instances for 'NonEmpty'
module Data.Aeson.Types.Instances.NonEmpty
  ( FromJSON (..),
    ToJSON (..),
    FromJSONKey (..),
    ToJSONKey (..),
  )
where

import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Data.NonEmpty

instance
  ( FromJSON a,
    Semigroup a,
    NonEmptySingleton a,
    NonEmptyFromContainer a,
    FromJSON (NonEmptySingletonElement a)
  ) =>
  FromJSON (NonEmpty a)
  where
  parseJSON x = do
    raw <- parseJSON x
    case nonEmpty raw of
      Just y -> pure y
      Nothing -> fail "parsing NonEmpty failed, unexpected empty container"

instance
  ( FromJSON a,
    NonEmptySingleton a,
    NonEmptyFromContainer a,
    FromJSON (NonEmptySingletonElement a),
    FromJSONKey (NonEmptySingletonElement a),
    FromJSONKey a,
    Semigroup a
  ) =>
  FromJSONKey (NonEmpty a)
  where
  fromJSONKey =
    case fromJSONKey @a of
      FromJSONKeyCoerce -> FromJSONKeyTextParser (run . coerce)
      FromJSONKeyText f -> FromJSONKeyTextParser (run . f)
      FromJSONKeyTextParser f -> FromJSONKeyTextParser (f >=> run)
      FromJSONKeyValue f -> FromJSONKeyValue (f >=> run)
    where
      run :: a -> Parser (NonEmpty a)
      run x =
        case nonEmpty x of
          Just y -> pure y
          Nothing -> fail "parsing NonEmpty failed, unexpected empty container"

instance (ToJSON a) => ToJSON (NonEmpty a) where
  toJSON = toJSON . getNonEmpty
  toEncoding = toEncoding . getNonEmpty
  toJSONList = toJSONList . map getNonEmpty
  toEncodingList = toEncodingList . map getNonEmpty

instance (ToJSONKey a) => ToJSONKey (NonEmpty a) where
  toJSONKey = contramapToJSONKeyFunction getNonEmpty toJSONKey
  toJSONKeyList = contramapToJSONKeyFunction (map getNonEmpty) toJSONKeyList
