-- |
-- Module        : Data.Text.Encoding.NonEmpty
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <foss@difolco.dev>
-- Stability     : Unstable
-- Portability   : GHC
--
-- 'NonEmpty' wrappers around `Data.Text.Encoding`
module Data.Text.Encoding.NonEmpty
  ( -- * Decoding ByteStrings to Text
    -- $strict
    decodeLatin1,
    decodeUtf8,
    decodeUtf16LE,
    decodeUtf16BE,
    decodeUtf32LE,
    decodeUtf32BE,

    -- ** Catchable failure
    decodeUtf8',

    -- ** Controllable error handling
    decodeUtf8With,
    decodeUtf16LEWith,
    decodeUtf16BEWith,
    decodeUtf32LEWith,
    decodeUtf32BEWith,

    -- ** Stream oriented decoding
    -- $stream
    streamDecodeUtf8,
    streamDecodeUtf8With,
    E.Decoding (..),

    -- * Encoding Text to ByteStrings
    encodeUtf8,
    encodeUtf16LE,
    encodeUtf16BE,
    encodeUtf32LE,
    encodeUtf32BE,

    -- * Encoding Text using ByteString Builders
    encodeUtf8Builder,
    encodeUtf8BuilderEscaped,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import Data.NonEmpty
import qualified Data.Text.Encoding as E
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException)
import Data.Text.NonEmpty
import Data.Word (Word8)
import GHC.Stack

-- $strict
--
-- All of the single-parameter functions for decoding bytestrings
-- encoded in one of the Unicode Transformation Formats (UTF) operate
-- in a /strict/ mode: each will throw an exception if given invalid
-- input.
--
-- Each function has a variant, whose name is suffixed with -'With',
-- that gives greater control over the handling of decoding errors.
-- For instance, 'decodeUtf8' will throw an exception, but
-- 'decodeUtf8With' allows the programmer to determine what to do on a
-- decoding error.

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
--
-- 'decodeLatin1' is semantically equivalent to
-- @Data.Text.pack . Data.ByteString.Char8.unpack@
--
-- This is a total function. However, bear in mind that decoding Latin-1 (non-ASCII)
-- characters to UTf-8 requires actual work and is not just buffer copying.
decodeLatin1 :: (HasCallStack) => NonEmpty ByteString -> NonEmptyStrictText
decodeLatin1 = overNonEmpty E.decodeLatin1
{-# INLINE decodeLatin1 #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With :: (HasCallStack) => OnDecodeError -> NonEmpty ByteString -> NonEmptyStrictText
decodeUtf8With onError = overNonEmpty $ E.decodeUtf8With onError
{-# INLINE decodeUtf8With #-}

-- | Decode, in a stream oriented way, a 'ByteString' containing UTF-8
-- encoded text that is known to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown (either by this function or a continuation) that cannot be
-- caught in pure code.  For more control over the handling of invalid
-- data, use 'streamDecodeUtf8With'.
streamDecodeUtf8 :: (HasCallStack) => NonEmpty ByteString -> NonEmpty E.Decoding
streamDecodeUtf8 = overNonEmpty E.streamDecodeUtf8
{-# INLINE streamDecodeUtf8 #-}

-- | Decode, in a stream oriented way, a lazy 'ByteString' containing UTF-8
-- encoded text.
streamDecodeUtf8With :: (HasCallStack) => OnDecodeError -> NonEmpty ByteString -> NonEmpty E.Decoding
streamDecodeUtf8With onError = overNonEmpty $ E.streamDecodeUtf8With onError
{-# INLINE streamDecodeUtf8With #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
--
-- This is a partial function: it checks that input is a well-formed
-- UTF-8 sequence and copies buffer or throws an error otherwise.
decodeUtf8 :: NonEmpty ByteString -> NonEmptyStrictText
decodeUtf8 = overNonEmpty E.decodeUtf8
{-# INLINE decodeUtf8 #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' :: (HasCallStack) => NonEmpty ByteString -> Either UnicodeException NonEmptyStrictText
decodeUtf8' = fmap trustedNonEmpty . E.decodeUtf8' . getNonEmpty
{-# INLINE decodeUtf8' #-}

-- | Encode text to a ByteString 'B.Builder' using UTF-8 encoding.
encodeUtf8Builder :: NonEmptyStrictText -> NonEmpty B.Builder
encodeUtf8Builder = overNonEmpty E.encodeUtf8Builder
{-# INLINE encodeUtf8Builder #-}

-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BP.BoundedPrim'.
--
-- Use this function is to implement efficient encoders for text-based formats
-- like JSON or HTML.
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> NonEmptyStrictText -> NonEmpty B.Builder
encodeUtf8BuilderEscaped be = overNonEmpty $ E.encodeUtf8BuilderEscaped be
{-# INLINE encodeUtf8BuilderEscaped #-}

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: NonEmptyStrictText -> NonEmpty ByteString
encodeUtf8 = overNonEmpty E.encodeUtf8
{-# INLINE encodeUtf8 #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> NonEmpty ByteString -> NonEmptyStrictText
decodeUtf16LEWith onError = overNonEmpty $ E.decodeUtf16LEWith onError
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: NonEmpty ByteString -> NonEmptyStrictText
decodeUtf16LE = overNonEmpty E.decodeUtf16LE
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> NonEmpty ByteString -> NonEmptyStrictText
decodeUtf16BEWith onError = overNonEmpty $ E.decodeUtf16BEWith onError
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
--
-- If the input contains any invalid big endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16BEWith'.
decodeUtf16BE :: NonEmpty ByteString -> NonEmptyStrictText
decodeUtf16BE = overNonEmpty E.decodeUtf16BE
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: NonEmptyStrictText -> NonEmpty ByteString
encodeUtf16LE = overNonEmpty E.encodeUtf16LE
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: NonEmptyStrictText -> NonEmpty ByteString
encodeUtf16BE = overNonEmpty E.encodeUtf16BE
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> NonEmpty ByteString -> NonEmptyStrictText
decodeUtf32LEWith onError = overNonEmpty $ E.decodeUtf32LEWith onError
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
--
-- If the input contains any invalid little endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32LEWith'.
decodeUtf32LE :: NonEmpty ByteString -> NonEmptyStrictText
decodeUtf32LE = overNonEmpty E.decodeUtf32LE
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> NonEmpty ByteString -> NonEmptyStrictText
decodeUtf32BEWith onError = overNonEmpty $ E.decodeUtf32BEWith onError
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
--
-- If the input contains any invalid big endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32BEWith'.
decodeUtf32BE :: NonEmpty ByteString -> NonEmptyStrictText
decodeUtf32BE = overNonEmpty E.decodeUtf32BE
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: NonEmptyStrictText -> NonEmpty ByteString
encodeUtf32LE = overNonEmpty E.encodeUtf32LE
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: NonEmptyStrictText -> NonEmpty ByteString
encodeUtf32BE = overNonEmpty E.encodeUtf32BE
{-# INLINE encodeUtf32BE #-}
