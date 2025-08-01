{-# LANGUAGE RecordWildCards #-}

module ChatCompletion.Providers.Google.Convert where

import ChatCompletion.Effect
import ChatCompletion.Providers.Google.Types
import ChatCompletion.Types
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static
import Relude

-- | Convert ChatMsg to GeminiContent
toGeminiContent :: ChatMsg -> Maybe GeminiContent
toGeminiContent msg = case msg of
    SystemMsg{} -> Nothing -- System messages are handled separately via systemInstruction
    UserMsg{content} ->
        Just $
            GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "user"
                }
    AssistantMsg{content} ->
        Just $
            GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "model"
                }
    ToolCallMsg{toolCalls} ->
        Just $
            GeminiContent
                { parts = V.fromList $ fmap translateToolCall toolCalls
                , role = "model"
                }
    ToolCallResponseMsg{toolCallId, toolResponse} ->
        Just $
            GeminiContent
                { parts =
                    V.singleton $
                        GeminiFunctionResponsePart $
                            GeminiFunctionResponse
                                { name = toolCallId ^. typed @Text
                                , response = object ["result" .= (toolResponse ^. #modelResponse)]
                                }
                , role = "user"
                }
  where
    translateToolCall :: ToolCall -> GeminiPart
    translateToolCall tc =
        GeminiFunctionCallPart $
            GeminiFunctionCall
                { name = tc ^. #toolName
                , args = Object (KM.fromMap (Map.mapKeys fromText (tc ^. #toolArgs)))
                }

-- | Convert ToolDeclaration to GeminiFunctionDeclaration
mkToolFromDeclaration :: ToolDeclaration -> GeminiFunctionDeclaration
mkToolFromDeclaration t =
    GeminiFunctionDeclaration
        { name = t ^. #name
        , description = t ^. #description
        , parameters = case t ^. #parameterSchema of
            Nothing -> Just $ object ["type" .= ("object" :: Text), "properties" .= object []]
            Just schema -> Just $ removeUnsupportedSchemaProps schema
        }
  where
    removeUnsupportedSchemaProps :: Value -> Value
    removeUnsupportedSchemaProps = \case
        Object obj -> Object $ KM.delete "additionalProperties" $ KM.map removeUnsupportedSchemaProps obj
        Array arr -> Array $ fmap removeUnsupportedSchemaProps arr
        v -> v

-- | Extract system message from message list
extractSystemMessage :: [ChatMsg] -> (Maybe GeminiContent, [ChatMsg])
extractSystemMessage msgs = case msgs of
    (SystemMsg content _ : rest) ->
        ( Just $
            GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "user" -- Gemini expects "user" role for system instruction
                }
        , rest
        )
    _ -> (Nothing, msgs)

-- | Convert GeminiContent to ChatMsg
fromGeminiContent
    :: Error ChatCompletionError :> es => UTCTime -> GeminiContent -> Eff es ChatMsg
fromGeminiContent now content = case content ^. #role of
    "model" -> case V.toList (content ^. #parts) of
        [GeminiTextPart text] ->
            pure $
                AssistantMsg
                    { content = text
                    , createdAt = now
                    }
        partsList | any isFunctionCall partsList -> do
            let functionCalls = [fc | GeminiFunctionCallPart fc <- partsList]
            pure $
                ToolCallMsg
                    { toolCalls = zipWith convertFunctionCallWithIndex [1..] functionCalls
                    , createdAt = now
                    }
        _ -> throwError $ ChatCompletionError "Unexpected model content structure"
    _ ->
        throwError $
            ChatCompletionError $
                "Unexpected role in response: " <> T.unpack (content ^. #role)
  where
    isFunctionCall (GeminiFunctionCallPart _) = True
    isFunctionCall _ = False

    convertFunctionCallWithIndex :: Int -> GeminiFunctionCall -> ToolCall
    convertFunctionCallWithIndex idx fc =
        ToolCall
            { toolCallId = ToolCallId $ fc ^. #name <> "_" <> T.pack (show idx) -- Add index for uniqueness
            , toolName = fc ^. #name
            , toolArgs = case fc ^. #args of
                Object km -> Map.mapKeys Key.toText (KM.toMap km)
                _ -> mempty -- If not an object, return empty map
            }