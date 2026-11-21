# r7-asyncapi

[AsyncAPI](https://www.asyncapi.com/) 3.0 documents in Haskell, with payloads
as [openapi3](https://hackage.haskell.org/package/openapi3) schemas, and
messages derived from your Haskell types.

`r7-asyncapi` is to AsyncAPI what `openapi3` is to OpenAPI: a data model of the
document, and a generic derivation that turns a Haskell type into its part of
the document. A sum type becomes one message per constructor, with the payload
that aeson encodes that constructor to. Schemas come from the `ToSchema`
instances of `openapi3` and are collected in its `Declare` monad, so an
AsyncAPI document and an OpenAPI document of the same service share them.

## Features

- A data model of the part of AsyncAPI 3.0 that describes a message stream:
  the document, its servers, channels, operations, messages and components,
  with lenses for every field.
- Payloads as `openapi3` schemas, under the OpenAPI 3.0 schema format.
- `ToAsyncMessages`: one message per constructor of a sum type, derived from
  its aeson `TaggedObject` encoding, checked at compile time where it can be.
- Helpers that add derived messages and schemas to a hand-written document,
  keeping the hand-written fields (titles, descriptions).
- Strict decoding: an unknown key is an error, so a hand-written document that
  decodes encodes back to itself.

## Installation

The package is `r7-asyncapi` and is not on Hackage yet. Add it as a dependency
from this repository.

With Stack, in `stack.yaml`:

```yaml
extra-deps:
  - github: rinn7e/haskell-package
    commit: <commit>
    subdirs:
      - package/r7-asyncapi
```

With cabal, in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/rinn7e/haskell-package.git
  tag: <commit>
  subdir: package/r7-asyncapi
```

Then add `r7-asyncapi` to the `build-depends` (or the `dependencies` of
`package.yaml`) of your package.

## Usage

Give a sum type a `TaggedObject` encoding and derive its messages with the
same aeson options, so the two cannot disagree:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Options (..), SumEncoding (..), ToJSON (..), defaultOptions, genericToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.AsyncApi.Messages (ToAsyncMessages (..), genericDeclareAsyncMessages)

data ChatMessage = ChatMessage { author :: Text, body :: Text }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

data Event = Connected | MessagePosted ChatMessage | UserLeft Text
  deriving stock (Generic)

eventOptions :: Options
eventOptions = defaultOptions { sumEncoding = TaggedObject "type" "data" }

instance ToJSON Event where
  toJSON = genericToJSON eventOptions

instance ToAsyncMessages Event where
  declareAsyncMessages = genericDeclareAsyncMessages eventOptions
```

Then add the messages to a document. `addChannelMessages` puts each message in
the components and lists it on the channel, and `addSchemas` adds the schemas
they use:

```haskell
import Control.Lens ((&))
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy (Proxy (..))

import Data.AsyncApi

document :: AsyncApi
document = base & addChannelMessages "events" messages & addSchemas schemas
  where
    (schemas, messages) = runDeclare (declareAsyncMessages (Proxy @Event)) mempty
```

Here `base` is an `AsyncApi` with an `events` channel (address `/events`),
built in Haskell or decoded from a hand-written JSON or YAML file. The encoded
`document` then has a message per constructor, for example:

```json
"MessagePosted": {
  "name": "MessagePosted",
  "payload": {
    "schemaFormat": "application/vnd.oai.openapi;version=3.0.0",
    "schema": {
      "type": "object",
      "properties": {
        "type": { "type": "string", "enum": ["MessagePosted"] },
        "data": { "$ref": "#/components/schemas/ChatMessage" }
      },
      "required": ["type", "data"]
    }
  }
}
```

For a type whose JSON is a single message, `declareMessage` gives a message
whose payload is the schema of that type.

### What the derivation takes

`genericDeclareAsyncMessages` needs every constructor to be an object of its
own:

- The sum encoding is `TaggedObject`.
- A constructor has no argument, or one argument without a field name (put
  the fields in a type of their own with its own `ToSchema`). A record
  constructor or several arguments is a type error.
- A `Maybe` argument is written as `null` when it is `Nothing`, so its schema
  takes `null` too.

Options that do not give every constructor an object of its own are an
`error` when the messages are declared: another `SumEncoding`, a single
constructor without `tagSingleConstructors`, only nullary constructors with
`allNullaryToStringTag`, or two constructors with the same tag after
`constructorTagModifier`.

### Hand-written and derived parts together

Two `Message` values combine field by field, the left one winning. So a
message written by hand in the document (its title and description) and the
derived one (its payload) become one message with both. A name the channel
already lists keeps its reference, a channel the document does not have is not
created, and a schema the document already has wins over a derived one.

## Modules

- `Data.AsyncApi`: the document model (`AsyncApi`, `Info`, `Server`,
  `Channel`, `Operation`, `Message`, `Components`, ...), its lenses, and
  `addChannelMessages`, `addComponentMessages` and `addSchemas`.
- `Data.AsyncApi.Messages`: `ToAsyncMessages`, `genericDeclareAsyncMessages`
  and `declareMessage`.

## Scope

The model covers the part of AsyncAPI 3.0 that describes a stream of
messages. Protocol bindings and message examples are kept as JSON values,
without a model of their own. Payload schemas are either plain schemas (read
as `openapi3` schemas) or OpenAPI 3.0 schemas; another schema format (Avro,
for example) is a decoding error.

## Development

From the root of the repository:

```bash
stack test r7-asyncapi
```

## License

MIT, see [LICENSE](LICENSE).
