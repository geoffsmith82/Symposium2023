# Artificial Intelligence and ChatGPT
Source code and Documentation for my [ADUG](https://www.adug.org.au) [Symposium Talk presented on the 28th of April 2023](https://www.youtube.com/watch?v=7HNiOMcHBW8). I have since added to and enhanced the code to further demonstrate capabilities of AI.

The goal of this project is to enable delphi users to be able to use AI technology in their applications.  There are many different types of AI and thousands of different models.  This project is working on creating generalized interfaces to the different types of AI models and make them easily accessible.

Artificial intelligence (AI) is an interdisciplinary field that combines computer science, mathematics, and cognitive psychology to create intelligent systems capable of performing complex tasks. Its rapid advancements have led to a wide array of applications demonstrating AI's versatility.

Language translation is one such application, where AI-powered tools efficiently translate between languages, simplifying tasks like translating software programs for global audiences. AI also excels in human-like conversations, with interactive applications that understand and respond to human language naturally. Voice recognition and real-time speech-to-text allow conversion and seamless voice-based interactions, making AI-driven applications more accessible and user-friendly.

In creative and artistic domains, AI can generate images based on textual descriptions, showcasing its capacity to understand and produce visual content. AI's computer vision capabilities enable it to accurately recognize faces and other objects in photographs and documents, illustrating its potential in visual recognition tasks and diverse applications like security and automation.

AI's ability to analyze and process data, and generate comprehensive reports highlights its value in various domains. Furthermore, AI-powered tools can transcribe audio files into written text, making transcription tasks more efficient and accurate.

The example programs below is an attempt to demonstrate the capabilities available to Delphi programmers today. I have worked on creating generic API's so that different providers can be swapped in or out to:
- experiment
- follow the current leading AI model
- make it easy to change based on price
- avoid vendor lockin
- or for any other reason.

## ChatGPT Prompts  

[Some Example GPT Prompts](./Documentation/Prompts.md) 

## Presentation Slides from Original 2023 ADUG Symposium
  - [AI and ChatGPT](./Documentation/AI%20and%20ChatGPT.pptx)
## Example programs
  - [ChatGPTAction](./ChatGPTAction) 
    - Simple Server app that can be added to a ChatGPT GPT action to allow your computer to write delphi code. See [AI Generated Delphi](https://github.com/geoffsmith82/AIGeneratedDelphi) for some example programs created with ChatGPTAction.
  - [EmbeddingsDemo](./EmbeddingsDemo)
	- Simple demo showing how Embeddings work
  - [Translate](./Translate)
	- translates between languages using the various cloud API's.  
	- Simplify translating Delphi programs when using Delphi's built-in multi language resource support.
  - [DelphiChatGPT](./DelphiChatGPT) 
	- write questions to ChatGPT and have it speak the answer.
	![image](./DelphiChatGPT/delphichatgpt.png)
  - [FaceDetection](./FaceDetection)  
	- Detect faces in a photo.
	![image](./FaceDetection/FaceDetectionScreenshot.png)	
  - [Weather](./Weather)  
	- Query the weather forcast for Bendigo from the bureau of meteorology generate a paragraph or two and read it out
	![image](./Weather/WeatherScreenshot.png)	
  - [TranscribeAudio](./TranscribeAudio)
    - Upload a audio file and have it translated via a cloud speech to text api.
  - [VoiceRecognition](./VoiceRecognition)
    - convert speech to text in real-time straight from your microphone, feed it to OpenAI's GPT and have the response 
	read back to you.
	![image](./VoiceRecognition/screenshot-hal9000.png)
  - [Image generation](./ImageGeneration)
    - generate an image using text that you provide using OpenAI's DALLE-2 and DALLE-3 API.
	
### Questions / Need Help?	
   Create an issue and I will respond to it.
	
### Providers Used/Available
  - Google - Text to Speech, LLM, Translate
  - Microsoft Azure - Text to Speech, GPT, Translate
  - Amazon - [Text to Speech](https://aws.amazon.com/polly/), [Translate](https://aws.amazon.com/translate/)
  - [Anthropic](https://www.anthropic.com) claude-3-opus, claude-3-sonnet and claude-3-haiku, supporting one of largest context windows currently available (200k tokens)
  - [Replicate](https://www.replicate.com) access a wide range of models
  - [Huggingface](https://huggingface.co/) access a wide range of models
  - [ElevenLabs](https://beta.elevenlabs.io/) Text to Speech and Voice Cloning
  - [OpenAI](https://platform.openai.com) Text to Speech, Whisper Voice Recognition, DALLE-2, DALLE-3 Image Generation, GPT4 LLM
  - [AssemblyAI](https://www.assemblyai.com/app) Voice Recognition
  - [DeepGram](https://deepgram.com/) Voice Recognition
  - [Rev.AI](https://www.rev.ai) Voice Recognition
  - [Conqui-ai](https://github.com/coqui-ai/TTS) Run a variaty of text to speech models locally from a docker container
  - [CodeProject-Ai](https://www.codeproject.com/AI/) Local Face Detection.
	
### Getting the projects working	
  - Each of the cloud API's need to have been setup in their respective developer consoles.  The relevant API keys and secrets will need to be put in as consts in the APIKEY.INC file.
  - A file in [/libs/APIKEY.INC.EXAMPLE](./Libs/apikey.inc.example) shows all the available keys to enter.  If you're not using a particular provider you don't need a key for it.

## Questions about code and how to set things up
  - Please feel free to raise issues about any questions you have about the code.  I know there is a lot to this project and lots to setup, so I would like to improve the documentation to make it easy for everyone to use all the parts of this project.

### Potential future areas of research/study
  - Using Embeddings to search large datasets
  - Using [Python4Delphi](https://github.com/pyscripter/python4delphi) to be able to call various Python AI libraries from Delphi.


### Artificial Intelligence Related links
- [TensorFlow.Delphi](https://github.com/Pigrecos/TensorFlow.Delphi)
- [whisper.cpp](https://github.com/ggerganov/whisper.cpp) Voice Recognition
- [llama.cpp](https://github.com/ggerganov/llama.cpp)
- [llama-pas](https://github.com/Kagamma/llama-pas)
- [Bing Image Creator](https://www.bing.com/images/create/)
- [Bing](https://www.bing.com/)
- [RDOpenAI](https://github.com/baumwollschaf/RDOpenAI) Delphi implementation of ChatGPT - an event based component 
- [ChatGPT](https://chat.openai.com/chat) OpenAI ChatGPT
- [DelphiOpenAI](https://github.com/HemulGM/DelphiOpenAI) a Delphi Library for OpenAI 
- [ChatGPTPluginForLazarus](https://github.com/AliDehbansiahkarbon/ChatGPTPluginForLazarus)  An OpenAI (ChatGPT) plug-in for Lazarus IDE. 
- [ChatGPT](https://github.com/HemulGM/ChatGPT) a Firemonkey ChatGPT interface written in Delphi.
- [AI-Playground-DesktopClient](https://github.com/FMXExpress/AI-Playground-DesktopClient) A Firemonkey Language model playground to access languages models like StableLM, ChatGPT, and more.
- [AI-Code-Translator](https://github.com/FMXExpress/AI-Code-Translator) Use GPT to translate between programming languages		
- [TOpenALPR](https://github.com/r1me/TOpenALPR) Open Source Number Plate recognition
- [PgVector](https://github.com/pgvector/pgvector) PgVector allows storing and querying of Vectors/Embeddings in an SQL database



- [CommonVoice](https://commonvoice.mozilla.org/en/languages) Public dataset of recordings for Voice Recognition
- [Huggingface](https://huggingface.co/)
- [Open-Assistant](https://open-assistant.io/)

### External Libraries required to build projects
 - [ICS 9.0](http://wiki.overbyte.eu/wiki/index.php/ICS_Download) or from the GetIt package manager built into Delphi. - For Voice recognition
 - [landgraf-dev/aws-sdk-delphi](https://github.com/landgraf-dev/aws-sdk-delphi) AWS SDK
 - [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework) for ChatGPTAction 

### Tools used to create example projects
 - https://github.com/PKGeorgiev/Delphi-JsonToDelphiClass
 - [Fiddler](https://www.fiddler.com)
 - XML Data Binder in Delphi