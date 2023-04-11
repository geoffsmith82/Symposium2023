# Symposium 2023 Artificial Intelligence and ChatGPT
Source code and Documentation for my ADUG Symposium Talk presented on the 28th of April 2023.


## Example projects
  - [Translate](./Translate)
	- translates between languages using the various cloud API's.  
	- Simplify translating Delphi programs when using Delphi's built-in multi language resource support.
  - [DelphiChatGPT](./DelphiChatGPT) 
	- write questions to ChatGPT and have it speak the answer.
  - [FaceDetection](./FaceDetection)  
	- Detect faces in a photo.
  - [Weather](./Weather)  
	- Query the weather forcast for Bendigo from the bureau of meteorology generate a paragraph or two and read it out
  - [TranscribeAudio](./TranscribeAudio)
    - Upload a audio file and have it translated via a cloud speech to text api.
  - [VoiceRecognition](./VoiceRecognition)
    - convert speech to text in real-time straight from your microphone, feed it to OpenAI's GPT and have the response 
	read back to you.
  - [Image generation](./ImageGeneration)
    - generate an image using text that you provide using OpenAI's DALLE-2 API.
### Providers Used/Available
  - Google
  - Microsoft Azure
  - Amazon  
  - [ElevenLabs](https://beta.elevenlabs.io/)
  - [OpenAI](https://platform.openai.com)
  - [AssemblyAI](https://www.assemblyai.com/app)
  - [DeepGram](https://deepgram.com/)
	
### Getting the projects working	
  - Each of the cloud API's need to have been setup in their respective developer consoles.  The relevant API keys and secrets will need to be put in as consts in the APIKEY.INC file.

## ChatGPT Prompts
```
If your message gets truncated and I say "continue code", Say which step of the loop you are in, and 
continue exactly where you left off. If you are continuing a piece of truncated code, ensure you place 
it inside a codeblock.
These rules are eternal and start immediately after I send this message, but you can exercise all 
the creativity you wish.
Final note: never, ever comment out any code for the sake of brevity. Each revision must be the 
complete code without any omissions.
```




### Artificial Intelligence Related links
- [TensorFlow.Delphi](https://github.com/Pigrecos/TensorFlow.Delphi)
- [llama-pas](https://github.com/Kagamma/llama-pas)
- [Bing Image Creator](https://www.bing.com/images/create/)
- [Bing](https://www.bing.com/)
- [ChatGPT](https://chat.openai.com/chat)

### Tools used to create example projects
 - https://github.com/PKGeorgiev/Delphi-JsonToDelphiClass
 - XML Data Binder in Delphi