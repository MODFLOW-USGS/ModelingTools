program SutraObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, FastGEO, BasisFunctionUnit, SubPolygonUnit, CustApp, InputFileReader
  { you can add units after this };

type

  { TSutraObsExtractor }

  TSutraObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSutraObsExtractor }

procedure TSutraObsExtractor.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TSutraObsExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSutraObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TSutraObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TSutraObsExtractor;
begin
  Application:=TSutraObsExtractor.Create(nil);
  Application.Title:='SUTRA Observation Extractor';
  Application.Run;
  Application.Free;
end.

