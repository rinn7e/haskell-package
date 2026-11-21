-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- | White space as JavaScript sees it. turndown's regular expressions use
-- @\\s@ and @String.prototype.trim@, whose set is not Haskell's 'Data.Char.isSpace'.
module Text.Turndown.Whitespace
  ( isJsSpace
  , jsTrim
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | JavaScript's @\\s@ (and the characters @String.prototype.trim@ removes).
isJsSpace :: Char -> Bool
isJsSpace c =
  c `elem` ("\t\n\v\f\r \x00A0\x1680\x2028\x2029\x202F\x205F\x3000\xFEFF" :: String)
    || (c >= '\x2000' && c <= '\x200A')

-- | JavaScript's @String.prototype.trim@.
jsTrim :: Text -> Text
jsTrim = T.dropAround isJsSpace
