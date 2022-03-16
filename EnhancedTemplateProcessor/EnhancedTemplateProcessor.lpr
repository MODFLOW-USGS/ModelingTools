program EnhancedTemplateProcessor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  ProcessTemplateUnit, DisclaimerTextUnit;

type

  { TEnhancedTemplateProcessor }

  TEnhancedTemplateProcessor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TEnhancedTemplateProcessor }

procedure TEnhancedTemplateProcessor.DoRun;
begin
  try
    ProcessTemplate;
  except on E:Exception do
    begin
      WriteLn(E.Message);
    end;
  end;

  // stop program loop
  Terminate;
end;

constructor TEnhancedTemplateProcessor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TEnhancedTemplateProcessor.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TEnhancedTemplateProcessor;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;

{$R *.res}

begin
  StartTime := Now;
  Application:=TEnhancedTemplateProcessor.Create(nil);
  try
  Application.Title:='Enhanced Template Processor';
    Application.Run;
  finally
    Application.Free;
  end;
  writeln('Normal termination');
  ElapsedTime := Now - StartTime;
  writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
end.

