program SwiObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, RealListUnit, SimpleTextWriter, SwiObsUtilities,
  SwiObsInputReader, SwiObsReaderUnit, DisclaimerTextUnit,
  InterpolatedObsResourceUnit, CustApp
  { you can add units after this };

type

  { TSwiObsExtractor }

  TSwiObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSwiObsExtractor }

procedure TSwiObsExtractor.DoRun;
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
  ExtractSwiObservations;

  // stop program loop
  Terminate;
end;

constructor TSwiObsExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSwiObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TSwiObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TSwiObsExtractor;
begin
  Application:=TSwiObsExtractor.Create(nil);
  Application.Title:='SwiObsExtractor';
  Application.Run;
  Application.Free;
end.

