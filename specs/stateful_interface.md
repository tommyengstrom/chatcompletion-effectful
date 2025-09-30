refactor the `respondWithToolsX` family of methods in LlmChat.hs to not use the LlmChatStorage backend.

## Example

```
respondWithTools
    :: ( HasCallStack
       , LlmChatStorage :> es
       , Time :> es
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
```

should get signature:

```
respondWithTools
    :: ( HasCallStack
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> [ChatMsg]
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
```

## Objective

- Refactor all function in LlmChat to get `[ChatMsg]` directly instead of using `LlmChatStorage`.
- create a function `withStorage` that fetch the conversation before and saves the results after :
```
withStorage
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => ([ChatMsg] -> Eff es a)
    -> ConversationId
    -> Eff es a
```
