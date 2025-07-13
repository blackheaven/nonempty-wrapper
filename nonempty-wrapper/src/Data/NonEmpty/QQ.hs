-- |
-- Module        : Data.NonEmpty.QQ
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- Create NonEmpty values from quasi-quoting instead of unsafe functions.
--
-- Since @0.1.1.0@
module Data.NonEmpty.QQ
  ( neString,

    -- * Re-export
    trustedNonEmpty,
  )
where

import Data.NonEmpty
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Build a NonEmpty 'IsString' safely.
--
--  > [neString|something|]
--
-- Since @0.1.1.0@
neString :: QuasiQuoter
neString =
  QuasiQuoter
    { quoteExp =
        \input ->
          if null input
            then fail "Cannot build a non-enpty String from an empty input"
            else return $ AppE (VarE $ mkName "trustedNonEmpty") $ LitE $ StringL input,
      quotePat = const $ fail "neString is only supported on Expressions",
      quoteType = const $ fail "neString is only supported on Expressions",
      quoteDec = const $ fail "neString is only supported on Expressions"
    }
