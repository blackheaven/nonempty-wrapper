{-# LANGUAGE CPP #-}
-- |
-- Module        : Data.NonEmpty.TH
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Unstable
-- Portability   : GHC
--
-- Create NonEmpty values from TemplateHaskell instead of unsafe functions.
--
-- Since @0.1.1.0@
module Data.NonEmpty.TH
  ( makeNonEmpty,

    -- * Re-export
    trustedNonEmpty,
  )
where

import Control.Monad (when)
import Data.NonEmpty
import Language.Haskell.TH

-- | Build a NonEmpty safely.
--
-- Since @0.1.1.0@
--
--  > $(makeNonEmpty [|"Hello"|])
--  > $(makeNonEmpty [|[1, 2]|])
makeNonEmpty :: Q Exp -> Q Exp
makeNonEmpty eExp = do
  e <- eExp
  let ensureNonEmpty =
        \case
          LitE (StringL s) ->
            when (null s) $
              fail "Cannot build a non-enpty value from an empty string"
          ListE es ->
            when (null es) $
              fail "Cannot build a non-enpty value from an empty list"
          SigE e' _ -> ensureNonEmpty e'
#if MIN_VERSION_base(4,19,0)
          TypedBracketE e' -> ensureNonEmpty e'
          TypedSpliceE e' -> ensureNonEmpty e'
#endif
          e' -> fail $ "Unsupported expression type: " <> show e'

  ensureNonEmpty e
  return $ AppE (VarE $ mkName "trustedNonEmpty") e
