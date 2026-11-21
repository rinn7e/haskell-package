-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

module Main
  ( main
  ) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Lens (_Just, ix, (&), (?~), (^.), (^?))
import Data.Aeson
  (FromJSON, Options(..), SumEncoding(ObjectWithSingleField, TaggedObject), ToJSON(toJSON),
  Value(Object, String), defaultOptions, eitherDecode, encode, genericToJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Either (isLeft, isRight)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi
  (Definitions, OpenApiType(OpenApiString), Referenced(..), Schema, ToSchema, type_)
import Data.OpenApi.Declare (Declare, runDeclare)
import Data.OpenApi.Schema.Validation (validateJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

import Data.AsyncApi
import Data.AsyncApi.Messages (ToAsyncMessages(..), genericDeclareAsyncMessages)

main :: IO ()
main = defaultMain $ testGroup "r7-asyncapi" [testDerivedMessages, testDocuments]

testDerivedMessages :: TestTree
testDerivedMessages = testGroup "derived messages"
  [ testCase "one message per constructor, named by its tag" $
      InsOrdHashMap.keys messages @?= ["Plain", "Named", "Nested", "Optional"]
  , testCase "each message names its tag" $
      map (^. messageName) (InsOrdHashMap.elems messages)
        @?= map Just ["Plain", "Named", "Nested", "Optional"]
  , testCase "the JSON of every constructor matches the payload of its message" $
      mapM_ (matchesMessage eventMessages)
        [Plain, Named "text", Nested (Inner 1 True), Optional (Just 2), Optional Nothing]
  , testCase "the tag of another constructor does not match a message" $
      -- The payload of Plain lists no contents field, so only its tag can
      -- reject the JSON of Named.
      case payloadOf messages "Plain" of
        Nothing -> assertFailure "no payload for Plain"
        Just schema -> assertBool "the JSON of Named matched the message of Plain" $
          not (null (validateJSON definitions schema (toJSON (Named "text"))))
  , testCase "the argument schema joins the definitions" $
      InsOrdHashMap.keys definitions @?= ["Inner"]
  , testCase "a tag modifier names the messages as aeson tags them" $ do
      InsOrdHashMap.keys (snd modifiedMessages) @?= ["A", "B"]
      mapM_ (matchesMessage modifiedMessages) [ModA 1, ModB]
  , testCase "a single constructor is a message with tagSingleConstructors" $
      matchesMessage singleMessages (Single 1)
  , testCase "a type with no constructors has no message" $
      InsOrdHashMap.size (snd (runDeclare (declareAsyncMessages (Proxy @Empty)) mempty)) @?= 0
  , testGroup "options that do not give each constructor an object are an error"
      [ testCase "another sum encoding" $
          isAnError $ genericDeclareAsyncMessages
            defaultOptions { sumEncoding = ObjectWithSingleField } (Proxy @Event)
      , testCase "a single constructor without tagSingleConstructors" $
          isAnError $ genericDeclareAsyncMessages (tagged defaultOptions) (Proxy @Single)
      , testCase "only nullary constructors with allNullaryToStringTag" $
          isAnError $ genericDeclareAsyncMessages (tagged defaultOptions) (Proxy @Colour)
      , testCase "two constructors with one tag" $
          isAnError $ genericDeclareAsyncMessages
            (tagged defaultOptions) { constructorTagModifier = const "Same" } (Proxy @Modified)
      ]
  ]

testDocuments :: TestTree
testDocuments = testGroup "documents"
  [ testCase "a document decodes and encodes back to itself" $
      (toJSON <$> (eitherDecode (encode document) :: Either String AsyncApi))
        @?= Right document
  , testCase "a key the model does not have is an error" $
      decodesTo (Proxy @Message) (object ["title" .= ("t" :: Text), "titel" .= ("t" :: Text)])
        @?= False
  , testCase "a reference with another key is an error" $
      decodesTo (Proxy @Ref)
        (object ["$ref" .= ("#/components/messages/A" :: Text), "summary" .= ("s" :: Text)])
        @?= False
  , testCase "an operation action is send or receive" $
      map (decodesTo (Proxy @OperationAction) . String) ["send", "receive", "publish"]
        @?= [True, True, False]
  , testCase "a payload of a schema format the model does not read is an error" $ do
      let payload format = object
            [ "payload" .= object
                ["schemaFormat" .= (format :: Text), "schema" .= object ["type" .= ("string" :: Text)]]
            ]
      decodesTo (Proxy @Message) (payload openApiSchemaFormat) @?= True
      decodesTo (Proxy @Message) (payload "application/vnd.apache.avro;version=1.9.0") @?= False
  , testCase "a channel lists the added messages, which keep their hand-written fields" $ do
      let doc = decoded & addChannelMessages "events" messages
      doc ^? eventsChannel . ix "Named" @?= Just (messageRef "Named")
      let named = doc ^? asyncApiComponents . _Just . componentsMessages . _Just . ix "Named"
      (named >>= (^. messageTitle)) @?= Just "Named, written by hand"
      (named >>= (^. messagePayload)) @?= (messages ^? ix "Named" >>= (^. messagePayload))
  , testCase "of two messages, the left one wins on a field both have" $
      (titled "left" <> titled "right") ^. messageTitle @?= Just "left"
  , testCase "a name the channel already lists keeps its reference" $ do
      let doc = decoded & addChannelMessages "events" (InsOrdHashMap.singleton "hello" mempty)
      doc ^? eventsChannel . ix "hello" @?= Just (messageRef "Hello")
  , testCase "a channel the document does not have is not created" $ do
      let doc = decoded & addChannelMessages "missing" messages
      doc ^? asyncApiChannels . _Just . ix "missing" @?= Nothing
      ((doc ^? asyncApiComponents . _Just . componentsMessages . _Just . ix "Named")
        >>= (^. messageName)) @?= Just "Named"
  , testCase "the document wins on a schema name both have" $ do
      let doc = decoded
            & addSchemas (InsOrdHashMap.singleton "Inner" (mempty & type_ ?~ OpenApiString))
            & addSchemas definitions
      ((doc ^? asyncApiComponents . _Just . componentsSchemas . _Just . ix "Inner")
        >>= (^. type_)) @?= Just OpenApiString
  ]
  where
    eventsChannel = asyncApiChannels . _Just . ix "events" . channelMessages . _Just
    titled :: Text -> Message
    titled title = mempty & messageTitle ?~ title

-- | A sum type with each kind of constructor the derivation takes.
data Event = Plain | Named Text | Nested Inner | Optional (Maybe Int)
  deriving stock (Generic)

data Inner = Inner { count :: Int, flag :: Bool }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

tagged :: Options -> Options
tagged options = options { sumEncoding = TaggedObject "_tag" "value" }

instance ToJSON Event where
  toJSON = genericToJSON (tagged defaultOptions)

instance ToAsyncMessages Event where
  declareAsyncMessages = genericDeclareAsyncMessages (tagged defaultOptions)

-- | Constructors whose tags drop a prefix.
data Modified = ModA Int | ModB
  deriving stock (Generic)

modifiedOptions :: Options
modifiedOptions = (tagged defaultOptions) { constructorTagModifier = drop 3 }

instance ToJSON Modified where
  toJSON = genericToJSON modifiedOptions

newtype Single = Single Int
  deriving stock (Generic)

singleOptions :: Options
singleOptions = (tagged defaultOptions) { tagSingleConstructors = True }

instance ToJSON Single where
  toJSON = genericToJSON singleOptions

data Colour = Red | Green
  deriving stock (Generic)

data Empty
  deriving stock (Generic)

instance ToAsyncMessages Empty

type Declared = (Definitions Schema, InsOrdHashMap Text Message)

eventMessages, modifiedMessages, singleMessages :: Declared
eventMessages = runDeclare (declareAsyncMessages (Proxy @Event)) mempty
modifiedMessages = runDeclare (genericDeclareAsyncMessages modifiedOptions (Proxy @Modified)) mempty
singleMessages = runDeclare (genericDeclareAsyncMessages singleOptions (Proxy @Single)) mempty

definitions :: Definitions Schema
messages :: InsOrdHashMap Text Message
(definitions, messages) = eventMessages

payloadOf :: InsOrdHashMap Text Message -> Text -> Maybe Schema
payloadOf declared name = case declared ^? ix name >>= (^. messagePayload) of
  Just (MultiFormat (MultiFormatSchema format (Inline schema))) | format == openApiSchemaFormat ->
    Just schema
  _ -> Nothing

-- | The JSON of the value matches the payload of the message its tag names.
matchesMessage :: ToJSON a => Declared -> a -> Assertion
matchesMessage (defs, declared) value = case json of
  Object o | Just (String tag) <- KeyMap.lookup "_tag" o, Just schema <- payloadOf declared tag ->
    validateJSON defs schema json @?= []
  _ -> assertFailure ("no OpenAPI payload for the tag of " <> show json)
  where
    json = toJSON value

isAnError :: Declare (Definitions Schema) (InsOrdHashMap Text Message) -> Assertion
isAnError declare = do
  result <- try @ErrorCall (evaluate (InsOrdHashMap.size (snd (runDeclare declare mempty))))
  assertBool "declared messages the JSON does not have" (isLeft result)

decodesTo :: forall a. FromJSON a => Proxy a -> Value -> Bool
decodesTo _ v = isRight (eitherDecode @a (encode v))

-- | A small document: a channel with one hand-written message, and the
-- hand-written part of a message the test derives.
document :: Value
document = object
  [ "asyncapi" .= ("3.0.0" :: Text)
  , "info" .= object ["title" .= ("Events" :: Text), "version" .= ("1.0.0" :: Text)]
  , "channels" .= object
      [ "events" .= object
          [ "address" .= ("/events" :: Text)
          , "messages" .= object ["hello" .= object ["$ref" .= ("#/components/messages/Hello" :: Text)]]
          ]
      ]
  , "components" .= object
      [ "messages" .= object
          [ "Hello" .= object
              [ "contentType" .= ("text/plain" :: Text)
              , "payload" .= object ["type" .= ("string" :: Text)]
              ]
          , "Named" .= object ["title" .= ("Named, written by hand" :: Text)]
          ]
      ]
  ]

decoded :: AsyncApi
decoded = either error id (eitherDecode (encode document))
