program EnhancedTemplateProcessor;

{#BACKUP EnhancedTemplateProcessor.lpr}
{#BACKUP EnhancedTemplateProcessor.lpi}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ProcessTemplateUnit in 'ProcessTemplateUnit.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

var
  StartTime: TDateTime;
  ElapsedTime: TDateTime;
begin
  StartTime := Now;
  try
    ProcessTemplate;
    { TODO -oUser -cConsole Main : Insert code here }
    writeln('Normal termination');
    ElapsedTime := Now - StartTime;
    writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      WriteErrorLog(E);
    end;
  end;
end.
