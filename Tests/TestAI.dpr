program TestAI;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  FastMM4,
  DUnitX.MemoryLeakMonitor.FastMM4,
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  TestBase.AI in 'TestBase.AI.pas',
  TestBase.LLM in 'TestBase.LLM.pas',
  TestBase.ImageGen in 'TestBase.ImageGen.pas',
  Test.LLM.Groq in 'Test.LLM.Groq.pas',
  Test.LLM.OpenAI in 'Test.LLM.OpenAI.pas',
  Test.ImageGen.Replicate in 'Test.ImageGen.Replicate.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.Anthropic in '..\Libs\LLM\uLLM.Anthropic.pas',
  uLLM.Azure in '..\Libs\LLM\uLLM.Azure.pas',
  uLLM.DeepSeek in '..\Libs\LLM\uLLM.DeepSeek.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.Google.Gemini in '..\Libs\LLM\uLLM.Google.Gemini.pas',
  uLLM.Google.PaLM in '..\Libs\LLM\uLLM.Google.PaLM.pas',
  uLLM.Groq in '..\Libs\LLM\uLLM.Groq.pas',
  uLLM.HuggingFace in '..\Libs\LLM\uLLM.HuggingFace.pas',
  uLLM.Mistral in '..\Libs\LLM\uLLM.Mistral.pas',
  uLLM.OpenAI.Assistants in '..\Libs\LLM\uLLM.OpenAI.Assistants.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM.OpenRouter in '..\Libs\LLM\uLLM.OpenRouter.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uLLM.Replicate in '..\Libs\LLM\uLLM.Replicate.pas',
  uLLM.X.Ai in '..\Libs\LLM\uLLM.X.Ai.pas',
  uDALLe2.DTO in '..\Libs\ImageGeneration\uDALLe2.DTO.pas',
  uImageGeneration.Imagen in '..\Libs\ImageGeneration\uImageGeneration.Imagen.pas',
  uImageGeneration.OpenAI in '..\Libs\ImageGeneration\uImageGeneration.OpenAI.pas',
  uImageGeneration in '..\Libs\ImageGeneration\uImageGeneration.pas',
  uImageGeneration.Replicate in '..\Libs\ImageGeneration\uImageGeneration.Replicate.pas',
  uImageGeneration.XAI in '..\Libs\ImageGeneration\uImageGeneration.XAI.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas';

{ keep comment here to protect the following conditional from being removed by the IDE when adding a unit }
{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
