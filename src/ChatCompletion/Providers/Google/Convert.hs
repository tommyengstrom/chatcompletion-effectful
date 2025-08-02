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

-- | Clean JSON response by removing markdown code blocks if present
-- Google often wraps JSON responses in ```json ... ``` blocks
cleanMarkdownJson :: Text -> Text
cleanMarkdownJson t =
    let stripped = T.strip t
        -- Remove ```json prefix
        withoutPrefix = if "```json" `T.isPrefixOf` stripped
                       then T.drop 7 stripped
                       else if "```" `T.isPrefixOf` stripped
                            then T.drop 3 stripped
                            else stripped
        -- Remove ``` suffix  
        withoutSuffix = if "```" `T.isSuffixOf` T.strip withoutPrefix
                       then T.dropEnd 3 (T.strip withoutPrefix)
                       else withoutPrefix
    in T.strip withoutSuffix

-- | Convert ChatMsg to GeminiContent
toGeminiContent :: ChatMsg -> Maybe GeminiContent
toGeminiContent msg = case msg of
    SystemMsg{} -> Nothing -- System messages are handled separately via systemInstruction
    UserMsg{content} ->
        Just
            $ GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "user"
                }
    AssistantMsg{content} ->
        Just
            $ GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "model"
                }
    ToolCallMsg{toolCalls} ->
        Just
            $ GeminiContent
                { parts = V.fromList $ fmap translateToolCall toolCalls
                , role = "model"
                }
    ToolCallResponseMsg{toolCallId, toolResponse} ->
        Just
            $ GeminiContent
                { parts =
                    V.singleton
                        $ GeminiFunctionResponsePart
                        $ GeminiFunctionResponse
                            { name = toolCallId ^. typed @Text
                            , response = object ["result" .= (toolResponse ^. #modelResponse)]
                            }
                , role = "user"
                }
  where
    translateToolCall :: ToolCall -> GeminiPart
    translateToolCall tc =
        GeminiFunctionCallPart
            $ GeminiFunctionCall
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
        ( Just
            $ GeminiContent
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
            pure
                $ AssistantMsg
                    { content = cleanMarkdownJson text
                    , createdAt = now
                    }
        partsList | any isFunctionCall partsList -> do
            let functionCalls = [fc | GeminiFunctionCallPart fc <- partsList]
            pure
                $ ToolCallMsg
                    { toolCalls = zipWith convertFunctionCallWithIndex [1 ..] functionCalls
                    , createdAt = now
                    }
        partsList -> 
            -- If we have multiple parts that aren't function calls, concatenate text parts
            -- This can happen when Google returns structured JSON after tool calls
            let textParts = [text | GeminiTextPart text <- partsList]
                combinedText = T.intercalate "" textParts
                -- Clean markdown code blocks that Google often adds around JSON
                cleanedText = cleanMarkdownJson combinedText
            in if null textParts
               then throwError $ ProviderError "Unexpected model content structure: no text or function calls"
               else pure $ AssistantMsg
                    { content = cleanedText
                    , createdAt = now
                    }
    _ ->
        throwError
            $ ProviderError
            $ "Unexpected role in response: "
            <> content
            ^. #role
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
