# chatcompletion-effectful

A Haskell library for building chat completion systems with OpenAI integration, persistent storage, and tool calling support.

## Design Goals

- **Easy to use**: Simple, intuitive API that makes chat completion straightforward
- **Leverage Effectful**: Built on modern Haskell effect system for composable, testable code
- **Practical**: Focused on real-world use cases with essential features

## Features

- **Chat Completion API**: High-level interface for conversational AI applications
- **Tool Calling**: Define and execute tools with JSON schema validation
- **Persistent Storage**: PostgreSQL and in-memory storage backends
- **OpenAI Integration**: Direct integration with OpenAI's chat completion API
- **Effect System**: Built on Effectful for composable, testable code
- **Response Logging**: Optional logging of OpenAI responses for debugging

## Quick Start

### Installation

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - chatcompletion-effectful
```

### Basic Usage

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import ChatCompletion
import ChatCompletion.OpenAI
import ChatCompletion.Storage.InMemory
import Effectful
import Effectful.Error.Static

main :: IO ()
main = runEff $ do
  runErrorNoCallStackWith (error . show) $ do
    runChatCompletionStorageInMemory $ do
      runErrorNoCallStackWith (error . show) $ do
        runChatCompletionOpenAi settings [] $ do
          -- Create a conversation with system prompt
          convId <- createConversation "You are a helpful assistant"
          
          -- Add user message and get response
          appendUserMessage convId "Hello! What's 2+2?"
          response <- respondToConversation convId
          
          liftIO $ print response
```

See [examples/Main.hs](examples/Main.hs) for complete working examples including simple conversation, calculator, and tool calling.


## Contributing

I've implemented the subset of features that I need. PRs for adding more features are welcome.
