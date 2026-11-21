-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- | A data model of the part of AsyncAPI 3.0 that describes a stream of
-- messages: the document, its servers, channels, operations and messages,
-- and its components.  Schemas are those of @openapi3@, so a type with a
-- 'Data.OpenApi.ToSchema' instance describes its payload, and the shapes it
-- shares with an OpenAPI document are written by the same code.
--
-- Protocol bindings and message examples have no model of their own: they
-- are kept as JSON as they are.
--
-- Every object of the model decodes strictly: a key it does not have is an
-- error.  Schemas are the exception: an @openapi3@ 'Schema' drops a key it
-- does not know.  So the check of a hand-written document is to decode it and
-- compare its encoding with the input, which then also catches a value the
-- model writes another way (an @address: null@, which it writes as no
-- address).
--
-- The field names follow the AsyncAPI 3.0.0 specification:
-- <https://www.asyncapi.com/docs/reference/specification/v3.0.0>.
module Data.AsyncApi
  ( -- * Document
    AsyncApi (..)
  , Info (..)
  , Server (..)
  , ServerVariable (..)
  , Channel (..)
  , Operation (..)
  , OperationAction (..)
  , Message (..)
  , Components (..)
    -- * Schemas and references
  , SchemaDefinition (..)
  , MultiFormatSchema (..)
  , Ref (..)
  , openApiSchemaFormat
  , openApiSchema
  , messageRef
    -- * Building a document
  , addComponentMessages
  , addChannelMessages
  , addSchemas
    -- * Lenses
  , asyncApiAsyncapi
  , asyncApiInfo
  , asyncApiDefaultContentType
  , asyncApiServers
  , asyncApiChannels
  , asyncApiOperations
  , asyncApiComponents
  , infoTitle
  , infoVersion
  , infoDescription
  , serverHost
  , serverProtocol
  , serverPathname
  , serverTitle
  , serverSummary
  , serverDescription
  , serverVariables
  , serverVariableEnum
  , serverVariableDefault
  , serverVariableDescription
  , serverVariableExamples
  , channelAddress
  , channelMessages
  , channelTitle
  , channelSummary
  , channelDescription
  , channelServers
  , channelBindings
  , operationAction
  , operationChannel
  , operationTitle
  , operationSummary
  , operationDescription
  , operationBindings
  , operationMessages
  , messageHeaders
  , messagePayload
  , messageContentType
  , messageName
  , messageTitle
  , messageSummary
  , messageDescription
  , messageBindings
  , messageExamples
  , componentsSchemas
  , componentsMessages
  , mfSchemaFormat
  , mfSchema
  ) where

import Control.Applicative ((<|>))
import Control.Lens (at, makeLenses, non, (%~), (&), (<>~))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withObject, withText, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi (Definitions, Referenced, Schema)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.AsyncApi.Internal.Json (objectOptions)

-- | A payload or headers schema: of the default format (an AsyncAPI schema,
-- read as an @openapi3@ 'Schema', which covers the plain schemas a document
-- writes by hand), or an OpenAPI 3.0 schema named with 'openApiSchemaFormat'.
-- A schema of another format is an error.
data SchemaDefinition
  = MultiFormat MultiFormatSchema
  | AsyncApiSchema (Referenced Schema)
  deriving stock (Eq, Show, Generic)

data MultiFormatSchema = MultiFormatSchema
  { _mfSchemaFormat :: Text
  , _mfSchema :: Referenced Schema
  } deriving stock (Eq, Show, Generic)

deriveJSON (objectOptions "_mf") ''MultiFormatSchema

instance ToJSON SchemaDefinition where
  toJSON = \case
    MultiFormat s -> toJSON s
    AsyncApiSchema s -> toJSON s

instance FromJSON SchemaDefinition where
  parseJSON v = case v of
    Object o | Just format <- KeyMap.lookup "schemaFormat" o ->
      if format == String openApiSchemaFormat
        then MultiFormat <$> parseJSON v
        else fail ("a schema format this model does not read: " <> show format)
    _ -> AsyncApiSchema <$> parseJSON v

-- | A reference to an object of the document, by its JSON pointer.
newtype Ref = Ref { refPath :: Text }
  deriving stock (Eq, Show, Generic)

instance ToJSON Ref where
  toJSON (Ref path) = object ["$ref" .= path]

instance FromJSON Ref where
  parseJSON = withObject "Reference" \o ->
    if KeyMap.size o == 1 then Ref <$> o .: "$ref" else fail "a reference has only $ref"

-- | The schema format of an OpenAPI 3.0 schema, which every message whose
-- payload is an @openapi3@ schema names.
openApiSchemaFormat :: Text
openApiSchemaFormat = "application/vnd.oai.openapi;version=3.0.0"

-- | An @openapi3@ schema as a payload or headers schema.
openApiSchema :: Referenced Schema -> SchemaDefinition
openApiSchema = MultiFormat . MultiFormatSchema openApiSchemaFormat

-- | The reference to the message of that name in the components.
messageRef :: Text -> Ref
messageRef name = Ref ("#/components/messages/" <> name)

-- | The root of a document.
data AsyncApi = AsyncApi
  { _asyncApiAsyncapi :: Text
    -- ^ The version of the specification, @3.0.0@.
  , _asyncApiInfo :: Info
  , _asyncApiDefaultContentType :: Maybe Text
  , _asyncApiServers :: Maybe (InsOrdHashMap Text Server)
  , _asyncApiChannels :: Maybe (InsOrdHashMap Text Channel)
  , _asyncApiOperations :: Maybe (InsOrdHashMap Text Operation)
  , _asyncApiComponents :: Maybe Components
  } deriving stock (Eq, Show, Generic)

data Info = Info
  { _infoTitle :: Text
  , _infoVersion :: Text
  , _infoDescription :: Maybe Text
  } deriving stock (Eq, Show, Generic)

data Server = Server
  { _serverHost :: Text
  , _serverProtocol :: Text
  , _serverPathname :: Maybe Text
  , _serverTitle :: Maybe Text
  , _serverSummary :: Maybe Text
  , _serverDescription :: Maybe Text
  , _serverVariables :: Maybe (InsOrdHashMap Text ServerVariable)
  } deriving stock (Eq, Show, Generic)

data ServerVariable = ServerVariable
  { _serverVariableEnum :: Maybe [Text]
  , _serverVariableDefault :: Maybe Text
  , _serverVariableDescription :: Maybe Text
  , _serverVariableExamples :: Maybe [Text]
  } deriving stock (Eq, Show, Generic)

-- | Where messages flow.  An absent 'channelAddress' means the address is
-- unknown or dynamic; the specification allows @null@ for that too, but the
-- model writes it as no address, so a document leaves it out.
data Channel = Channel
  { _channelAddress :: Maybe Text
  , _channelMessages :: Maybe (InsOrdHashMap Text Ref)
  , _channelTitle :: Maybe Text
  , _channelSummary :: Maybe Text
  , _channelDescription :: Maybe Text
  , _channelServers :: Maybe [Ref]
  , _channelBindings :: Maybe Value
  } deriving stock (Eq, Show, Generic)

data OperationAction = Send | Receive
  deriving stock (Eq, Show, Generic)

instance ToJSON OperationAction where
  toJSON = \case
    Send -> "send"
    Receive -> "receive"

instance FromJSON OperationAction where
  parseJSON = withText "OperationAction" \case
    "send" -> pure Send
    "receive" -> pure Receive
    other -> fail ("unknown operation action: " <> show other)

data Operation = Operation
  { _operationAction :: OperationAction
  , _operationChannel :: Ref
  , _operationTitle :: Maybe Text
  , _operationSummary :: Maybe Text
  , _operationDescription :: Maybe Text
  , _operationBindings :: Maybe Value
  , _operationMessages :: Maybe [Ref]
  } deriving stock (Eq, Show, Generic)

-- | A message.  Two messages combine field by field, the left one winning, so
-- a hand-written message (its title and description) and a derived one (its
-- payload) make one with both.
data Message = Message
  { _messageHeaders :: Maybe SchemaDefinition
  , _messagePayload :: Maybe SchemaDefinition
  , _messageContentType :: Maybe Text
  , _messageName :: Maybe Text
  , _messageTitle :: Maybe Text
  , _messageSummary :: Maybe Text
  , _messageDescription :: Maybe Text
  , _messageBindings :: Maybe Value
  , _messageExamples :: Maybe [Value]
  } deriving stock (Eq, Show, Generic)

instance Semigroup Message where
  l <> r = Message
    { _messageHeaders = _messageHeaders l <|> _messageHeaders r
    , _messagePayload = _messagePayload l <|> _messagePayload r
    , _messageContentType = _messageContentType l <|> _messageContentType r
    , _messageName = _messageName l <|> _messageName r
    , _messageTitle = _messageTitle l <|> _messageTitle r
    , _messageSummary = _messageSummary l <|> _messageSummary r
    , _messageDescription = _messageDescription l <|> _messageDescription r
    , _messageBindings = _messageBindings l <|> _messageBindings r
    , _messageExamples = _messageExamples l <|> _messageExamples r
    }

instance Monoid Message where
  mempty = Message Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Reusable objects.  Two components combine their schemas (the left one
-- winning on a name both have) and their messages (combined by name).
data Components = Components
  { _componentsSchemas :: Maybe (Definitions Schema)
    -- ^ In the slot of AsyncAPI schemas, but the derived ones are OpenAPI 3.0
    -- schemas, read through the format of the payloads that refer to them.
  , _componentsMessages :: Maybe (InsOrdHashMap Text Message)
  } deriving stock (Eq, Show, Generic)

instance Semigroup Components where
  l <> r = Components
    { _componentsSchemas = _componentsSchemas l <> _componentsSchemas r
    , _componentsMessages = unionMaybe (_componentsMessages l) (_componentsMessages r)
    }
    where
      unionMaybe (Just a) (Just b) = Just (InsOrdHashMap.unionWith (<>) a b)
      unionMaybe a b = a <|> b

instance Monoid Components where
  mempty = Components Nothing Nothing

foldMap (\(name, prefix) -> deriveJSON (objectOptions prefix) name)
  [ (''AsyncApi, "_asyncApi")
  , (''Info, "_info")
  , (''Server, "_server")
  , (''ServerVariable, "_serverVariable")
  , (''Channel, "_channel")
  , (''Operation, "_operation")
  , (''Message, "_message")
  , (''Components, "_components")
  ]

foldMap makeLenses
  [ ''AsyncApi
  , ''Info
  , ''Server
  , ''ServerVariable
  , ''Channel
  , ''Operation
  , ''Message
  , ''Components
  , ''MultiFormatSchema
  ]

-- | Combines these messages into the messages of the components (a message
-- the document has gets the fields it lacks).
addComponentMessages :: InsOrdHashMap Text Message -> AsyncApi -> AsyncApi
addComponentMessages messages =
  asyncApiComponents . non mempty <>~ Components Nothing (Just messages)

-- | 'addComponentMessages', and lists each message on the channel of that
-- name, under its own name (a name the channel already lists keeps its
-- reference).  A channel the document does not have is not created, so the
-- messages are then in the components only.
addChannelMessages :: Text -> InsOrdHashMap Text Message -> AsyncApi -> AsyncApi
addChannelMessages channel messages doc = addComponentMessages messages doc
  & asyncApiChannels . non mempty . at channel . traverse . channelMessages . non mempty
      %~ (<> InsOrdHashMap.mapWithKey (\name _ -> messageRef name) messages)

-- | Adds these schemas to the components (a schema the document has wins).
addSchemas :: Definitions Schema -> AsyncApi -> AsyncApi
addSchemas schemas = asyncApiComponents . non mempty <>~ Components (Just schemas) Nothing
