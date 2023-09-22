# TOpenAI Class Documentation

The `TOpenAI` class is a wrapper around the OpenAI API that allows users to interact with AI models, such as ChatGPT, DALL-E, and more. This class inherits from the `TBaseOpenAI` class and provides methods for sending chat messages, generating images, and obtaining embeddings.

## TChatSettings Record

The `TChatSettings` record is a structure that contains configuration settings for interacting with the ChatGPT model:

```pascal
TChatSettings = record
  model : string;
  temperature : Double;
  top_p : Double;
  n : Integer;
  stop : string;
  max_tokens : Integer;
  presence_penalty : Double;
  frequency_penalty : Double;
  user : string;
end;
```

- `model`: The identifier of the ChatGPT model to use.
- `temperature`: Affects the randomness of the model's output. Higher values make output more random, while lower values make it more deterministic.
- `top_p`: Controls the nucleus sampling method, where only tokens with a cumulative probability above this value are considered for sampling.
- `n`: The number of completions to generate for each prompt.
- `stop`: A string that, when encountered, causes the model to stop generating text.
- `max_tokens`: The maximum number of tokens to generate in the response.
- `presence_penalty`: Penalty applied to new tokens based on how often they appear in the text.
- `frequency_penalty`: Penalty applied to tokens based on their frequency in the training dataset.
- `user`: The identifier for the user sending the messages.

## TOpenAI Methods

### Constructor

```pascal
constructor Create(APIKey: string);
```

Create a new `TOpenAI` object with the given API key.

#### Example 1: Initializing the TOpenAI class

```pascal
var
  OpenAI: TOpenAI;

begin
  OpenAI := TOpenAI.Create('your-api-key');
  try
    // ...
  finally
    OpenAI.Free;
  end;
end;
```

### ListOpenAIModels

```pascal
procedure ListOpenAIModels(out AModelList: TStringList);
```

Retrieve a list of available OpenAI models and store them in `AModelList`.



#### Example 2: Listing available OpenAI models

```pascal
var
  OpenAI: TOpenAI;
  ModelList: TStringList;

begin
  OpenAI := nil;
  ModelList := nil;
  try
    OpenAI := TOpenAI.Create('your-api-key');
    ModelList := TStringList.Create;

    OpenAI.ListOpenAIModels(ModelList);

    // Print the list of models
    WriteLn(ModelList.Text);
  finally
    ModelList.Free;
    OpenAI.Free;
  end;
end;
```

### ChatCompletion

```pascal
function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
```

Send a series of chat messages to the ChatGPT model with the specified `ChatConfig` settings. Returns a `TChatResponse` object containing the AI's response.

#### Example 3: Sending chat messages to ChatGPT

```pascal
var
  OpenAI: TOpenAI;
  ChatConfig: TChatSettings;
  ChatMessages: TObjectList<TChatMessage>;
  ChatMessage: TChatMessage;
  Response: TChatResponse;

begin
  OpenAI := TOpenAI.Create('your-api-key');
  try
    // Configure the chat settings
    ChatConfig.model := 'text-chatbot-001';
    ChatConfig.temperature := 0.8;
    ChatConfig.top_p := 0.9;
    ChatConfig.n := 1;
    ChatConfig.stop := '';
    ChatConfig.max_tokens := 100;
    ChatConfig.presence_penalty := 0.6;
    ChatConfig.frequency_penalty := 0.5;
    ChatConfig.user := 'user1';

    // Add a message to the list
    ChatMessages := nil;
    ChatMessage := nil;
    Response := nil;
    try
      ChatMessages := TObjectList<TChatMessage>.Create;  
      ChatMessage :=  TChatMessage.Create('user', 'What is the capital of France?');	  
      ChatMessages.Add(ChatMessage);

      // Send messages to ChatGPT
      Response := OpenAI.ChatCompletion(ChatConfig, ChatMessages);

      // Print the response
      WriteLn(Response.Text);
    finally
      ChatMessages.Free;
      ChatMessage.Free;
      Response.Free;
    end;
  finally
    OpenAI.Free;
  end;
end;
```


### CallDALL_E

```pascal
function CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
```

Generate images using the DALL-E model with the given `prompt`, `n` number of images to generate, and `size` for the generated images. Returns a `TGeneratedImagesClass` object containing the generated images.

#### Example 4: Generating images using DALL-E

```pascal
var
  OpenAI: TOpenAI;
  Images: TGeneratedImagesClass;

begin
  OpenAI := TOpenAI.Create('your-api-key');
  try
    // Generate images with DALL-E
    Images := OpenAI.CallDALL_E('A futuristic city skyline at night', 3, TDALLESize.Large);

    try
      for i := 0 to High(images.data) do
      begin
        var imageURL : string := images.data[i].url;
        // Download, Process and display the images
        // ...
		
      end;

    finally
      Images.Free;
    end;
  finally
    OpenAI.Free;
  end;
end;
```



### AskChatGPT

```pascal
function AskChatGPT(const AQuestion: string; const AModel: string): string; override;
```

Ask the ChatGPT model a question using the given `AQuestion` and `AModel` identifier. Returns the AI's response as a string.

### Embeddings

```pascal
function Embeddings(const Texts: TArray<string>): TEmbeddings;
```

Get embeddings for the given array of `Texts`. Returns a `TEmbeddings` object containing the embeddings.

### Example 5: Getting embeddings for a list of texts

```pascal
var
  OpenAI: TOpenAI;
  Texts: TArray<string>;
  EmbeddingList: TEmbeddings;
begin
  OpenAI := TOpenAI.Create('your-api-key');
  try
    // Define the texts for which you want to get embeddings
    Texts := ['apple', 'banana', 'grape'];

    // Get embeddings
    EmbeddingList := OpenAI.Embeddings(Texts);

    try
      // Process and display the embeddings
      // ...
    finally
      EmbeddingList.Free;
    end;
  finally
    OpenAI.Free;
  end;
end;
```