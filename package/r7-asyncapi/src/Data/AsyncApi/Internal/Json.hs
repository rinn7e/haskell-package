-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- | The aeson options of the AsyncAPI objects, in a module of their own
-- because the instances are derived with Template Haskell.
module Data.AsyncApi.Internal.Json
  ( objectOptions
  ) where

import Data.Aeson (Options(..), defaultOptions)
import Data.Char (toLower)

-- | The options of an object whose fields are named @_\<prefix\>\<Key\>@:
-- the JSON key is @\<key\>@, an absent optional field is left out, and
-- decoding rejects a key the object does not have, so a document that
-- decodes encodes back to itself.
objectOptions :: String -> Options
objectOptions prefix = defaultOptions
  { fieldLabelModifier = lowerFirst . drop (length prefix)
  , omitNothingFields = True
  , rejectUnknownFields = True
  }
  where
    lowerFirst = \case
      c : cs -> toLower c : cs
      [] -> []
