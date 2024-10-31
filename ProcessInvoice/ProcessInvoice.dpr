program ProcessInvoice;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.OpenAI.Assistants in '..\Libs\LLM\uLLM.OpenAI.Assistants.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM in '..\Libs\LLM\uLLM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
