program EnhancedTemplateProcessor;

{#BACKUP EnhancedTemplateProcessor.lpr}
{#BACKUP EnhancedTemplateProcessor.lpi}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ProcessTemplateUnit in 'ProcessTemplateUnit.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

begin
  try
    ProcessTemplate;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
