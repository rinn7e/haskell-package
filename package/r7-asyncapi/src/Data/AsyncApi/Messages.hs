-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- A ticked type operator (@':<>:@) reads as a suffix use to this warning.
{-# OPTIONS_GHC -Wno-operator-whitespace #-}

-- | Messages derived from Haskell types, the way @openapi3@ derives schemas:
-- one message per constructor of a sum type, its payload the JSON aeson
-- encodes that constructor to, and the schemas it uses declared in the
-- 'Declare' monad of @openapi3@, so they can join the components of the
-- document (or of an OpenAPI document).
module Data.AsyncApi.Messages
  ( ToAsyncMessages (..)
  , GToAsyncMessages
  , genericDeclareAsyncMessages
  , declareMessage
  ) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (Options(..), SumEncoding(..), Value(Null, String))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Kind (Constraint, Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.OpenApi
  (Definitions, OpenApiType(OpenApiObject, OpenApiString), Referenced(Inline), Schema, ToSchema,
  declareSchemaRef, enum_, oneOf, properties, required, type_)
import Data.OpenApi.Declare (Declare)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
  (C1, Constructor(conName), D1, Generic(Rep), K1, Meta(MetaCons, MetaSel), S1, U1, V1, type (:*:),
  type (:+:))
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import Data.AsyncApi (Message, messageName, messagePayload, openApiSchema)

-- | Types whose JSON is one of several messages, each with a name.
class ToAsyncMessages a where
  -- | The messages, by name, with the schemas they use declared.
  declareAsyncMessages :: Proxy a -> Declare (Definitions Schema) (InsOrdHashMap Text Message)
  default declareAsyncMessages
    :: GToAsyncMessages (Rep a)
    => Proxy a -> Declare (Definitions Schema) (InsOrdHashMap Text Message)
  declareAsyncMessages = genericDeclareAsyncMessages Aeson.defaultOptions

-- | The messages of a sum type whose 'Aeson.ToJSON' is the generic one with
-- these options, which must encode it as a 'TaggedObject': each constructor
-- is a message named by its tag, whose payload is an object with the tag
-- and, for a constructor with an argument, the argument under the contents
-- field.  Pass the options the 'Aeson.ToJSON' instance uses, so the two
-- cannot disagree.
--
-- A constructor has at most one argument, without a field name: aeson writes
-- the fields of a record constructor next to the tag, and the fields of
-- several arguments as an array, and neither is a schema of its own.  Either
-- is a type error.  A 'Maybe' argument is written as @null@ when it is
-- 'Nothing', so its schema takes @null@ too.  Options that do not give every
-- constructor an object of its own (another 'SumEncoding', a single
-- constructor without 'tagSingleConstructors', only nullary constructors with
-- 'allNullaryToStringTag', two constructors with one tag after
-- 'constructorTagModifier') are an 'error' when the messages are declared.
genericDeclareAsyncMessages
  :: forall a. GToAsyncMessages (Rep a)
  => Options -> Proxy a -> Declare (Definitions Schema) (InsOrdHashMap Text Message)
genericDeclareAsyncMessages options _ = case sumEncoding options of
  TaggedObject tagField contentsField
    -- A type with no constructors has no value, so no JSON.
    | null constructors -> pure mempty
    | [_] <- constructors, not (tagSingleConstructors options) ->
        unsupported "a single constructor is not tagged without tagSingleConstructors"
    | all (isNothing . conArgument) constructors, allNullaryToStringTag options ->
        unsupported "nullary constructors are strings with allNullaryToStringTag"
    | tag : _ <- duplicates (map tagOf constructors) ->
        unsupported ("two constructors have the tag " <> show tag)
    | otherwise ->
        InsOrdHashMap.fromList <$> traverse (message (T.pack tagField) (T.pack contentsField)) constructors
  _ -> unsupported "only a TaggedObject sum encoding has one object per constructor"
  where
    constructors = gConstructors (Proxy @(Rep a))
    unsupported reason = error ("genericDeclareAsyncMessages: " <> reason)
    tagOf con = T.pack (constructorTagModifier options (conTag con))
    duplicates tags = [ tag | (tag, n) <- Map.toList (Map.fromListWith (+) [ (t, 1 :: Int) | t <- tags ]), n > 1 ]
    message tagField contentsField con = do
      argument <- sequence (conArgument con)
      let tag = tagOf con
          payload = mempty
            & type_ ?~ OpenApiObject
            & properties .~ InsOrdHashMap.fromList
                ( (tagField, Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ [String tag]))
                : [ (contentsField, schema) | Just schema <- [argument] ]
                )
            & required .~ (tagField : [ contentsField | Just _ <- [argument] ])
      pure (tag, mempty & messageName ?~ tag & messagePayload ?~ openApiSchema (Inline payload))

-- | A message whose payload is the JSON of @a@.
declareMessage :: ToSchema a => Proxy a -> Declare (Definitions Schema) Message
declareMessage p = do
  schema <- declareSchemaRef p
  pure (mempty & messagePayload ?~ openApiSchema schema)

-- | A constructor: its name, and the schema of its argument when it has one.
data Con = Con
  { conTag :: String
  , conArgument :: Maybe (Declare (Definitions Schema) (Referenced Schema))
  }

-- | The constructors of a generic representation.
type GToAsyncMessages :: (Type -> Type) -> Constraint
class GToAsyncMessages f where
  gConstructors :: Proxy f -> [Con]

instance GToAsyncMessages f => GToAsyncMessages (D1 d f) where
  gConstructors _ = gConstructors (Proxy @f)

instance GToAsyncMessages V1 where
  gConstructors _ = []

instance (GToAsyncMessages f, GToAsyncMessages g) => GToAsyncMessages (f :+: g) where
  gConstructors _ = gConstructors (Proxy @f) <> gConstructors (Proxy @g)

instance Constructor c => GToAsyncMessages (C1 c U1) where
  gConstructors _ = [Con (conName (Of @c @U1)) Nothing]

instance (Constructor c, ToSchema t) => GToAsyncMessages (C1 c (S1 ('MetaSel 'Nothing su ss ds) (K1 i t))) where
  gConstructors _ = [Con (conName (Of @c @U1)) (Just (declareSchemaRef (Proxy @t)))]

-- | openapi3 gives a 'Maybe' the schema of its content, made for an optional
-- field, but aeson writes 'Nothing' under the contents field as @null@.
instance {-# OVERLAPPING #-} (Constructor c, ToSchema t)
  => GToAsyncMessages (C1 c (S1 ('MetaSel 'Nothing su ss ds) (K1 i (Maybe t)))) where
  gConstructors _ = [Con (conName (Of @c @U1)) (Just nullable)]
    where
      nullable = do
        schema <- declareSchemaRef (Proxy @t)
        -- A @null@ branch with no @type@, which would exclude @null@.
        pure (Inline (mempty & oneOf ?~ [schema, Inline (mempty & enum_ ?~ [Null])]))

instance TypeError (NotOneArgument name "a record field: aeson writes it next to the tag")
  => GToAsyncMessages (C1 ('MetaCons name fx s) (S1 ('MetaSel ('Just field) su ss ds) f)) where
  gConstructors _ = []

instance TypeError (NotOneArgument name "several arguments: aeson writes them as an array")
  => GToAsyncMessages (C1 ('MetaCons name fx s) (f :*: g)) where
  gConstructors _ = []

type NotOneArgument :: Symbol -> Symbol -> ErrorMessage
type NotOneArgument name what =
  'Text "ToAsyncMessages: the constructor " ':<>: 'Text name ':<>: 'Text " has " ':<>: 'Text what
    ':$$: 'Text "Give it one argument of a type with its own ToSchema instance."

-- | Stands for a constructor of metadata @c@, to read its name.
type Of :: Meta -> (Type -> Type) -> Type -> Type
data Of c f p = Of
