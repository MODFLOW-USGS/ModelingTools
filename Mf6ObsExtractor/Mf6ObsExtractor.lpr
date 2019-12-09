program Mf6ObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, OutputFileReader;

type

  { TMf6ObsExtractor }

  TMf6ObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMf6ObsExtractor }

procedure TMf6ObsExtractor.DoRun;
var
  ErrorMsg: String;
  Dict: TObservationDictionary;
  OutputFile: TOutputFile;
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

  Dict := TObservationDictionary.Create;
  OutputFile := TOutputFile.Create('C:\ModelingTools\ModelMuse\Mf6_ObsExample\Mf6_ObsExample2.ob_gw_out_head.bin', ftBinary, Dict);
  try
    OutputFile.ReadTimeAndValues;
    OutputFile.ReadTimeAndValues;
  finally
    OutputFile.Free;
    Dict.Free;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TMf6ObsExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMf6ObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TMf6ObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMf6ObsExtractor;
begin
  Application:=TMf6ObsExtractor.Create(nil);
  Application.Title:='MODFLOW 6 Observation Extractor';
  Application.Run;
  Application.Free;
end.

